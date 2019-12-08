module Lorentz.Contracts.Multisig.Parameter
  ( Order
  , Parameter (..)
  , Signatures
  , ValueToSign (..)

  , mkCallOrder
  , mkCallOrderUnsafe
  , mkRotateKeysOrder
  , mkTransferOrder
  , mkTransferOrderUnsafe
  ) where

import Lorentz

import Lorentz.Contracts.Multisig.Error ()

type Signatures = [(PublicKey, Signature)]

data Parameter = Parameter
  { order :: Order
  , nonce :: Natural
  , signatures :: Signatures
  } deriving stock Generic
    deriving anyclass IsoValue

instance ParameterEntryPoints Parameter where
  parameterEntryPoints = pepNone

-- | Action that is going to be executed. May be either some
-- contract call, or key rotation.
data Order
  = Call (Lambda () Operation)
  | RotateKeys (Set KeyHash)
  deriving stock Generic
  deriving anyclass IsoValue

-- | Value that the participants should sign. Includes nonce to
-- prevent replay attacks and chain id to prevent cross-chain
-- replays.
data ValueToSign = ValueToSign
  { vtsChainId :: ChainId
  , vtsNonce :: Natural
  , vtsOrder :: Order
  } deriving stock Generic
    deriving anyclass IsoValue

-- | Make an order to transfer @amount@ utz from multisig to
-- @address@ passing @param@ as the transaction parameter.
mkTransferOrder
  :: forall a. (NiceParameter a, NiceConstant a)
  => Mutez -> ContractRef a -> a -> Order
mkTransferOrder value targetContract param =
  mkTransferOrderUnsafe value (fromContractAddr targetContract) param

-- | An unsafe version of @mkTransferOrder@ that accepts
-- Address instead of ContractRef.
mkTransferOrderUnsafe
  :: forall a. (NiceParameter a, NiceConstant a)
  => Mutez -> Address -> a -> Order
mkTransferOrderUnsafe value targetAddress param = Call $ do
  drop
  push targetAddress
  contract @a
  if IsSome
  then nop
  else failCustom_ #invalidStakerContract
  push value
  push param
  transferTokens

-- | Make an order to "call" an entrypoint, which is equal
-- to transferring 0 from multisig to @address@ passing
-- @param@ as the transaction parameter.
mkCallOrder
  :: forall a. (NiceParameter a, NiceConstant a)
  => ContractRef a -> a -> Order
mkCallOrder = mkTransferOrder (toMutez 0)

-- | An unsafe version of @mkCallOrder@ that accepts Address
-- instead of ContractRef.
mkCallOrderUnsafe
  :: forall a. (NiceParameter a, NiceConstant a)
  => Address -> a -> Order
mkCallOrderUnsafe = mkTransferOrderUnsafe (toMutez 0)

mkRotateKeysOrder :: Set KeyHash -> Order
mkRotateKeysOrder = RotateKeys
