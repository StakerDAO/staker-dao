module Lorentz.Contracts.Multisig
  ( Parameter (..)
  , Storage (..)

  , multisigContract
  ) where

import Lorentz

import Lorentz.Contracts.Common ()
import qualified Lorentz.Contracts.STKR as STKR


type Signatures = [(PublicKey, Signature)]

data Storage = Storage
  { keys :: Set KeyHash
  , quorum :: Natural
  , currentNonce :: Natural
  , stakerAddress :: Address
  } deriving stock Generic
    deriving anyclass IsoValue

data Parameter = Parameter
  { stakerParam :: STKR.Parameter
  , nonce :: Natural
  , signatures :: Signatures
  } deriving stock Generic
    deriving anyclass IsoValue

-- | When this contract is called, it checks that signatures are OK
-- and invokes the Staker contract with the supplied parameter.
-- Callable by: Operations team provided the majority quorum is met.
multisigContract :: '[(Parameter, Storage)] :-> '[([Operation], Storage)]
multisigContract = do
  unpair
  dup; dip checkNonce
  dip dup; dup
  dip checkSignatures
  dip $ do
    getField #stakerAddress
    contract @(STKR.Parameter)
    if IsSome
    then nop
    else failCustom_ #invalidStakerContract
    push (toMutez 0)
  toField #stakerParam
  transferTokens
  dip nil; cons; pair

checkNonce :: '[Parameter, Storage] :-> '[Storage]
checkNonce = do
  dip $ do
    getField #currentNonce
    push @Natural 1
    add
    dup
  toField #nonce
  if IsEq
  then setField #currentNonce
  else failCustom_ #invalidNonce

checkSignatures :: forall s. Parameter ': Storage ': s :-> s
checkSignatures = do
  ensureQuorum
  getField #signatures
  dip $ do
    dip $ toField #keys
    getField #nonce
    dip $ toField #stakerParam
    chainId

  stackType @(Signatures : ChainId : Natural : STKR.Parameter : Set KeyHash : s)
  dip $ pair # pair # pack
  stackType @(Signatures : ByteString : Set KeyHash : s)
  iter $ do
    stackType @((PublicKey, Signature) : ByteString : Set KeyHash : s)
    unpair;
    ensureSignatureValid
    dip $ duupX @2
    ensureKeyEligible
    stackType @(ByteString : Set KeyHash : s)
  stackType @(ByteString : Set KeyHash : s)
  dropN @2
  where
    ensureQuorum :: Parameter ': Storage ': s1 :-> Parameter ': Storage ': s1
    ensureQuorum = do
      dip $ getField #quorum
      getField #signatures
      size
      dip swap
      ge
      if Holds
      then nop
      else failCustom_ #quorumNotReached

    ensureSignatureValid
      :: PublicKey : Signature : ByteString : s1 :-> PublicKey : ByteString : s1
    ensureSignatureValid = do
      duupX @3; dig @2; duupX @3
      stackType @(PublicKey : Signature : ByteString : PublicKey : ByteString : _)
      checkSignature
      if Holds
      then nop
      else failCustom #invalidSignature

    ensureKeyEligible
      :: PublicKey : Set KeyHash : s1 :-> s1
    ensureKeyEligible = do
      dup; hashKey
      dip swap
      mem
      if Holds
      then drop
      else failCustom #invalidSignature

type instance ErrorArg "invalidNonce" = ()

instance (CustomErrorHasDoc "invalidNonce") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "The supplied nonce is invalid"

  customErrArgumentSemantics =
    Just "the supplied nonce is invalid"

type instance ErrorArg "invalidStakerContract" = ()

instance (CustomErrorHasDoc "invalidStakerContract") where
  customErrClass = ErrClassActionException

  customErrDocMdCause =
    "The stored staker contract address does not point to a valid STKR contract"

  customErrArgumentSemantics =
    Just "the stored staker contract address is invalid"
