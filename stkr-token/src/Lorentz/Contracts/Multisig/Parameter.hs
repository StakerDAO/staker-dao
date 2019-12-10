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

data Parameter a = Parameter
  { order :: Order a
  , nonce :: Natural
  , signatures :: Signatures
  } deriving stock Generic
    deriving anyclass IsoValue

instance NiceParameter a => ParameterEntryPoints (Parameter a) where
  parameterEntryPoints = pepNone

-- | Action that is going to be executed. May be either some
-- contract call, or key rotation.
data Order a
  = Call (a, (Lambda a Operation))
  | RotateKeys (Set KeyHash)
  deriving stock Generic
  deriving anyclass IsoValue

-- | Value that the participants should sign. Includes nonce to
-- prevent replay attacks and chain id to prevent cross-chain
-- replays.
data ValueToSign a = ValueToSign
  { vtsChainId :: ChainId
  , vtsNonce :: Natural
  , vtsOrder :: Order a
  } deriving stock Generic
    deriving anyclass IsoValue

-- | Make an order to transfer @amount@ utz from multisig to
-- @address@ passing @param@ as the transaction parameter.
mkTransferOrder
  :: NiceParameter a
  => Mutez -> ContractRef a -> a -> Order a
mkTransferOrder value targetContract param =
  mkTransferOrderUnsafe value (fromContractAddr targetContract) param

-- | An unsafe version of @mkTransferOrder@ that accepts
-- Address instead of ContractRef.
mkTransferOrderUnsafe
  :: forall a. NiceParameter a
  => Mutez -> Address -> a -> Order a
mkTransferOrderUnsafe value targetAddress param = Call . (param,) $ do
  dip $ do
    push targetAddress
    contract @a
    if IsSome
    then nop
    else failCustom_ #invalidStakerContract
    push value
  transferTokens

-- | Make an order to "call" an entrypoint, which is equal
-- to transferring 0 from multisig to @address@ passing
-- @param@ as the transaction parameter.
mkCallOrder
  :: NiceParameter a
  => ContractRef a -> a -> Order a
mkCallOrder = mkTransferOrder (toMutez 0)

-- | An unsafe version of @mkCallOrder@ that accepts Address
-- instead of ContractRef.
mkCallOrderUnsafe
  :: NiceParameter a
  => Address -> a -> Order a
mkCallOrderUnsafe = mkTransferOrderUnsafe (toMutez 0)

mkRotateKeysOrder :: Set KeyHash -> Order a
mkRotateKeysOrder = RotateKeys
