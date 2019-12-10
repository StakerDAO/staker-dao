module Lorentz.Contracts.Multisig
  ( Order
  , Parameter (..)
  , Signatures
  , Storage (..)
  , ValueToSign (..)

  , mkCallOrder
  , mkCallOrderUnsafe
  , mkRotateKeysOrder
  , mkTransferOrder
  , mkTransferOrderUnsafe
  , multisigContract
  ) where

import Lorentz

import Lorentz.Contracts.Common
import Lorentz.Contracts.Multisig.Error ()
import Lorentz.Contracts.Multisig.Parameter
import Lorentz.Contracts.Multisig.Storage

multisigContract
  :: forall a. NicePackedValue a
  => '[(Parameter a, Storage)] :-> '[([Operation], Storage)]
multisigContract = do
  unpair
  dup; dip updateNonceIfCorrect
  dupTop2; checkSignatures
  toField #order
  caseT $
    ( #cCall /-> do
        unpair; exec
        dip nil; cons; pair

    , #cRotateKeys /-> do
        setField #teamKeys
        nil; pair
    )

-- | Ensures nonce is equal to (currentNonce + 1) and updates currentNonce
-- if the condition holds. Otherwise, fails with #invalidNonce
updateNonceIfCorrect
  :: forall a s. NicePackedValue a
  => Parameter a ': Storage ': s :-> Storage ': s
updateNonceIfCorrect = do
  dip $ do
    getField #currentNonce
    push @Natural 1
    add
    dup
  toField #nonce
  if IsEq
  then setField #currentNonce
  else failCustom_ #invalidNonce

-- | Ensures that there's enough signatures, checks each of the supplied
-- signatures, fails if the quorum is not met or if any of the signatures
-- is invalid.
checkSignatures
  :: forall a s. NicePackedValue a
  => Parameter a ': Storage ': s :-> s
checkSignatures = do
  dup; prepareData

  swap; toField #signatures
  dipN @2 $ toField #teamKeys

  stackType @(Signatures : ByteString : Set KeyHash : s)
  map $ do
    unpair;
    stackType @(PublicKey : Signature : ByteString : Set KeyHash : s)
    duupX @4; duupX @2; ensureKeyEligible
    dipN @2 dup; ensureSignatureValid

  stackType @([KeyHash] : ByteString : Set KeyHash : s)
  dip drop
  listToSet
  toNamed #supplied
  dip $ toNamed #eligible
  ensureQuorum

  where
    ensureQuorum :: ("supplied" :! Set KeyHash) ': ("eligible" :! Set KeyHash) ': s1 :-> s1
    ensureQuorum = do
      dip $ do
        fromNamed #eligible
        size
      fromNamed #supplied
      size
      push @Natural 2
      mul  -- 2 * supplied signatures > eligible keys count,
      gt   -- note the strict inequality
      if Holds
      then nop
      else failCustom_ #majorityQuorumNotReached

    prepareData :: Parameter a : s1 :-> ByteString : s1
    prepareData = do
      constructT @(ValueToSign a) $
        ( fieldCtor $ chainId
        , fieldCtor $ getField #nonce
        , fieldCtor $ getField #order
        )
      dip drop
      stackType @(ValueToSign a : _)
      pack

    ensureSignatureValid
      :: PublicKey : Signature : ByteString : s1 :-> KeyHash ': s1
    ensureSignatureValid = do
      dup; dip checkSignature
      swap
      if Holds
      then hashKey
      else failCustom #invalidSignature

    ensureKeyEligible :: PublicKey : Set KeyHash : s1 :-> s1
    ensureKeyEligible = do
      dup; hashKey
      dip swap
      mem
      if Holds
      then drop
      else failCustom #invalidSignature
