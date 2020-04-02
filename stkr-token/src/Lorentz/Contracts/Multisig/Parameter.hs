module Lorentz.Contracts.Multisig.Parameter
  ( Order
  , Parameter (..)
  , Signatures
  , ValueToSign (..)
  , OrderDest (..)
  , Label

  , mkCallOrder
  , mkCallOrderWrap
  , mkRotateKeysOrder
  , mkTransferOrder
  , TransferOrderWrapC
  , mkTransferOrderWrap
  ) where

import Lorentz hiding (Call)

import Michelson.Typed.Haskell.Instr.Sum
  (CtorField(..), GetCtorField, InstrWrapC)

import Lorentz.Contracts.Multisig.Error ()

type Signatures = [(PublicKey, Signature)]

data Parameter = Parameter
  { order :: Order
  , nonce :: Natural
  , signatures :: Signatures
  } deriving stock Generic
    deriving anyclass (IsoValue, HasTypeAnn)

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdNone

-- | Action that is going to be executed. May be either some
-- contract call, or key rotation.
data Order
  = Call (Lambda () Operation)
  | RotateKeys (Set KeyHash)
  deriving stock Generic
  deriving anyclass (IsoValue, HasTypeAnn)

-- | Value that the participants should sign. Includes nonce to
-- prevent replay attacks and the multisig address to prevent both
-- cross-address and cross-chain replays.
data ValueToSign = ValueToSign
  { vtsMultisigAddress :: Address
  , vtsNonce :: Natural
  , vtsOrder :: Order
  } deriving stock Generic
    deriving anyclass IsoValue

data OrderDest a
  = Unsafe Address
  | Ref (TAddress a)


type ReceiverParam a =
  ( NiceParameterFull a
  , ForbidExplicitDefaultEntryPoint a
  )

mkTransferOrderGeneric
  :: forall a. ReceiverParam a
   => Mutez -> OrderDest a -> (forall st. st :-> a ': st) -> Order
mkTransferOrderGeneric value orderDest paramSupplier = Call $ do
  drop
  push $
    case orderDest of
      Unsafe addr -> addr
      Ref taddr -> unTAddress taddr
  contract @a
  if IsSome
  then nop
  else failCustom_ #invalidStakerContract
  push value
  paramSupplier
  transferTokens

-- | Make an order to transfer @amount@ utz from multisig to
-- @address@ passing @param@ as the transaction parameter.
mkTransferOrder
  :: forall a. (ReceiverParam a, NiceConstant a)
  => Mutez -> OrderDest a -> a -> Order
mkTransferOrder value dest param =
  mkTransferOrderGeneric value dest (push param)

type TransferOrderWrapC dt cName it =
  ( InstrWrapC dt cName
  , ReceiverParam dt
  , GetCtorField dt cName ~ 'OneField it
  , NiceConstant it
  )

-- | A version of @mkTransferOrder@ that accepts sum type branch label
-- and its constructor argument, should be used when downstream contract
-- parameter type does not satisfy NiceConstant constraint
mkTransferOrderWrap
  :: forall dt cName it.
  TransferOrderWrapC dt cName it
  => Mutez -> OrderDest dt -> Label cName -> it -> Order
mkTransferOrderWrap value dest label param =
  mkTransferOrderGeneric value dest $ do
    push param
    wrap_ @dt label

-- | Make an order to "call" an entrypoint, which is equal
-- to transferring 0 from multisig to @address@ passing
-- @param@ as the transaction parameter.
mkCallOrder
  :: forall a. (ReceiverParam a, NiceConstant a)
  => OrderDest a -> a -> Order
mkCallOrder = mkTransferOrder (toMutez 0)

mkCallOrderWrap
  :: forall dt cName it.
  TransferOrderWrapC dt cName it
  => OrderDest dt -> Label cName -> it -> Order
mkCallOrderWrap = mkTransferOrderWrap (toMutez 0)

mkRotateKeysOrder :: Set KeyHash -> Order
mkRotateKeysOrder = RotateKeys
