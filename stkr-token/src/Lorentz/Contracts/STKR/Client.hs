module Lorentz.Contracts.STKR.Client
  ( DeployOptions(..)
  , deploy

  , CallOptions(..)
  , call

  , AlmostStorage(..)
  , getStorage
  ) where

import Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Set as S

import Fmt (Buildable(..), Builder, blockMapF, jsonListF, mapF')

import Tezos.Address (Address)
import Tezos.Core (unsafeMkMutez)
import Tezos.Crypto (formatKeyHash)
import Tezos.Crypto (PublicKey, KeyHash, hashKey)
import Util.Named ((:!))

import Lorentz (IsoValue, Lambda, Operation)

import qualified Lorentz.Contracts.STKR as STKR
import TzTest (TzTest)
import qualified TzTest as Tz

data DeployOptions = DeployOptions
  { contractAlias :: Text
  , originator :: Address
  , councilPks :: [PublicKey]
  , teamMultisig :: Address
  , timeConfig :: STKR.TimeConfig
  }

deploy :: DeployOptions -> TzTest Address
deploy DeployOptions{..} = do
  let initStorage =
        STKR.Storage
          { owner = teamMultisig
          , councilKeys = S.fromList (hashKey <$> councilPks)
          , proposals = Map.empty
          , lastProposalId = 0
          , votes = Map.empty
          , policy = #urls Map.empty
          , stageCounter = 0
          , totalSupply = 0
          , ledger = mempty
          , frozen = False
          , successor = Nothing
          }
  Tz.originateContract $
    Tz.OriginateContractP
      { ocpAlias = contractAlias
      , ocpQty = 0
      , ocpSrc = originator
      , ocpContract = STKR.stkrContract timeConfig
      , ocpInitalStorage = initStorage
      , ocpBurnCap = 220
      }

data AlmostStorage = AlmostStorage
  { owner :: Address
  , councilKeys :: Set KeyHash
  , policy :: STKR.Policy
  , proposals :: [STKR.ProposalAndHash]
  , votes :: Map KeyHash ("proposalId" :! Natural)
  , stageCounter :: Natural
  -- ^ @stageCounter `div` 4@ is current epoch and @stageCounter `mod` 4@
  -- denotes current stage within an epoch
  , totalSupply :: Natural
  , ledger :: Natural
  -- ^ Big maps represented as their BigMapId's in tezos-client output
  , frozen :: Bool
  , successor :: Maybe (Lambda STKR.PublicEntrypointParam Operation)
  }
  deriving stock Generic
  deriving anyclass IsoValue

instance Buildable AlmostStorage where
  build AlmostStorage{..} = blockMapF @[(Text, Builder)] $
    [ ("owner", build owner)
    , ("councilKeys", jsonListF councilKeys)
    , ("policy", build policy)
    , ("proposals", jsonListF proposals)
    , ("votes", mapF' (build . formatKeyHash) build votes)
    , ("stageCounter", build stageCounter)
    , ("totalSupply", build totalSupply)
    , ("ledger", "BigMap values should not be displayed")
    ]

getStorage :: Address -> TzTest AlmostStorage
getStorage = Tz.getStorage @AlmostStorage

data CallOptions = CallOptions
  { caller :: Address
  , contract :: Address
  , parameter :: STKR.Parameter
  }

call
  :: CallOptions
  -> TzTest ()
call CallOptions{..} = Tz.transfer $
  Tz.TransferP
    { tpQty = unsafeMkMutez 0
    , tpSrc = caller
    , tpDst = contract
    , tpBurnCap = 22
    , tpArgument = parameter
    }
