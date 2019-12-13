{-# OPTIONS_GHC -Wno-orphans #-}

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

import Lorentz.Contracts.Common (dupTop2)
import Lorentz.Contracts.Multisig.Common
import Lorentz.Contracts.Multisig.Error ()
import Lorentz.Contracts.Multisig.Parameter
import Lorentz.Contracts.Multisig.Storage

multisigContract :: Contract Parameter Storage
multisigContract = do
  unpair
  dup; dip updateNonceIfCorrect
  dupTop2; checkSignatures
  toField #order
  caseT $
    ( #cCall /-> do
        unit; exec
        dip nil; cons; pair

    , #cRotateKeys /-> do
        setField #teamKeys
        nil; pair
    )
