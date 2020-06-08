module Client.Contracts.STKR
  ( DeployOptions(..)
  , deploy

  , CallOptions(..)
  , call

  , AlmostStorage(..)
  , getStorage
  , reservoirAddr
  ) where

import Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Set as S

import Fmt (Buildable(..), Builder, blockMapF, jsonListF, mapF')

import Lorentz.CryptoInterop (KeyHash, PublicKey, formatKeyHash, hashKey)
import Tezos.Address (Address, unsafeParseAddress)
import Tezos.Core (unsafeMkMutez)
import Util.Named ((:!))

import Lorentz (BigMap(..), IsoValue, Lambda, Operation)

import Client.Tezos (TzEnv)
import qualified Client.Tezos as Tz
import qualified Lorentz.Contracts.STKR as STKR

data DeployOptions = DeployOptions
  { contractAlias :: Text
  , originator :: Address
  , councilPks :: [PublicKey]
  , teamMultisig :: Address
  , timeConfig :: STKR.TimeConfig
  , totalSupply_ :: Natural
  }

-- | Fixed address needed for deployment, should not be changed in future
reservoirAddr :: Address
reservoirAddr = unsafeParseAddress "tz1WAVpSaCFtLQKSJkrdVApCQC1TNK8iNxq9"

deploy :: DeployOptions -> TzEnv Address
deploy DeployOptions{..} = do
  let initStorage =
        STKR.Storage
          { owner = teamMultisig
          , councilKeys = S.fromList (hashKey <$> councilPks)
          , proposals = []
          , votes = Map.empty
          , policy = #urls Map.empty
          , stageCounter = 0
          , totalSupply = totalSupply_
          , ledger = BigMap $ Map.singleton reservoirAddr totalSupply_
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

getStorage :: Address -> TzEnv AlmostStorage
getStorage = Tz.getStorage @AlmostStorage

data CallOptions = CallOptions
  { caller :: Address
  , contract :: Address
  , parameter :: STKR.Parameter
  }

call
  :: CallOptions
  -> TzEnv ()
call CallOptions{..} = Tz.transfer $
  Tz.TransferP
    { tpQty = unsafeMkMutez 0
    , tpSrc = caller
    , tpDst = contract
    , tpBurnCap = 22
    , tpArgument = parameter
    }
