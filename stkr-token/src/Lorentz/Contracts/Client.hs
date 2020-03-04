{-# LANGUAGE NoRebindableSyntax #-}

module Lorentz.Contracts.Client
  ( DeployOptions (..)
  , deploy
  , multisignBytes
  , multisignValue
  , signBytes
  , ContractAddresses (..)
  , ViaMultisigOptions (..)
  , callViaMultisig
  , signViaMultisig
  , VoteForProposalOptions (..)
  , voteForProposal
  , voteForProposalSig
  , fund
  , getTotalSupply
  , getBalance
  , mkStkrOpsOrder
  , mkStkrFrozenOrder
  ) where

import Prelude

import Fmt (Buildable(..), Builder, mapF)
import Lens.Micro (ix)
import Named (arg)

import Lorentz.Constraints (NicePackedValue)
import Lorentz.Pack (lPackValue)
import Tezos.Address (Address)
import Tezos.Core (Mutez)
import Tezos.Crypto (KeyHash, PublicKey, SecretKey, Signature, sign, toPublic)

import TzTest (TzTest)
import qualified TzTest as Tz

import qualified Lorentz.Contracts.Multisig as Msig
import qualified Lorentz.Contracts.Multisig.Client as Msig
import qualified Lorentz.Contracts.STKR as STKR
import qualified Lorentz.Contracts.STKR.Client as STKR

data DeployOptions = DeployOptions
  { msigAlias :: Text
  , stkrAlias :: Text
  , originator :: Address
  , councilPks :: [PublicKey]
  , teamKeys :: Set KeyHash
  , timeConfig :: STKR.TimeConfig
  , totalSupply_ :: Natural
  }

data ContractAddresses = ContractAddresses
  { stkrAddr :: Address
  , msigAddr :: Address
  }

instance Buildable ContractAddresses where
  build ContractAddresses{..} = mapF @[(Text, Builder)] $
    [ ("token", build stkrAddr)
    , ("multisig", build msigAddr)
    ]

deploy :: DeployOptions -> TzTest ContractAddresses
deploy DeployOptions{..} = do
  msigAddr <- Msig.deploy $ Msig.DeployOptions
    { contractAlias = msigAlias
    , teamKeys = teamKeys
    , ..
    }
  stkrAddr <- STKR.deploy $ STKR.DeployOptions
    { contractAlias = stkrAlias
    , teamMultisig = msigAddr
    , ..
    }
  pure ContractAddresses{..}

multisignValue
  :: NicePackedValue a
  => [SecretKey] -- Sks to be signed with
  -> a           -- Value to be signed
  -> [(PublicKey, Signature)]
multisignValue opsSks = multisignBytes opsSks . lPackValue

multisignBytes
  :: [SecretKey] -- Sks to be signed with
  -> ByteString  -- Value to be signed
  -> [(PublicKey, Signature)]
multisignBytes opsSks bytes =
  opsSks <&> flip signBytes bytes

signBytes
  :: SecretKey -- Sk to be signed with
  -> ByteString  -- Value to be signed
  -> (PublicKey, Signature)
signBytes sk bytes =
  (toPublic sk, sign sk bytes)

signViaMultisig
  :: Msig.Order
  -> ViaMultisigOptions
  -> TzTest (Msig.Parameter, [(PublicKey, Signature)])
signViaMultisig order ViaMultisigOptions {..} = do
  let getNonce = (+1) . Msig.currentNonce <$> Tz.getStorage vmoMsig
  nonce <- maybe getNonce pure vmoNonce
  let toSign = Msig.ValueToSign vmoMsig nonce order
  let bytes = lPackValue toSign
  pkSigs <- vmoSign bytes
  let param = Msig.Parameter order nonce pkSigs
  pure (param, pkSigs)

mkStkrFrozenOrder
  :: STKR.PermitOnFrozenParam
  -> Address
  -> Msig.Order
mkStkrFrozenOrder stkrParam stkrAddr =
  Msig.mkCallOrderWrap @STKR.Parameter
    (Msig.Unsafe stkrAddr) #cPermitOnFrozen
    (STKR.EnsureOwner stkrParam)

mkStkrOpsOrder
  :: STKR.OpsTeamEntrypointParam
  -> Address
  -> Msig.Order
mkStkrOpsOrder stkrParam stkrAddr =
  Msig.mkCallOrderWrap @STKR.Parameter
    (Msig.Unsafe stkrAddr) #cOpsTeamEntrypoint
    (STKR.EnsureOwner stkrParam)

callViaMultisig
  :: Address -> Msig.Order -> ViaMultisigOptions -> TzTest ()
callViaMultisig from order vmo = do
  (param, _) <- signViaMultisig order vmo
  Tz.call from (vmoMsig vmo) param

data ViaMultisigOptions = ViaMultisigOptions
  { vmoMsig :: Address
  , vmoSign :: ByteString -> TzTest [(PublicKey, Signature)]
  , vmoNonce :: Maybe Natural
  }

data VoteForProposalOptions = VoteForProposalOptions
  { vpStkr :: Address
  , vpFrom :: Address
  , vpSign :: ByteString -> TzTest (PublicKey, Signature)
  , vpEpoch :: Natural
  , vpProposalId :: Natural
  }

voteForProposalSig
  :: VoteForProposalOptions -> TzTest (PublicKey, Signature)
voteForProposalSig VoteForProposalOptions {..} = do
  STKR.AlmostStorage{..} <- STKR.getStorage vpStkr
  proposalHash <-
    maybe (fail $ "Proposal id not found " <> show vpProposalId) pure .
    fmap (arg #proposalHash . snd) $ proposals ^? ix (fromIntegral vpProposalId)
  let curStage = vpEpoch*4 + 2
  let toSignB = lPackValue $ STKR.CouncilDataToSign proposalHash vpStkr curStage
  vpSign toSignB

voteForProposal :: VoteForProposalOptions -> TzTest ()
voteForProposal vp@VoteForProposalOptions {..} = do
  (pk, sig) <- voteForProposalSig vp
  Tz.call vpFrom vpStkr
    $ STKR.PublicEntrypoint
    . STKR.VoteForProposal
    $ (#proposalId vpProposalId, #votePk pk, #voteSig sig)

fund :: Address -> Address -> Mutez -> ByteString -> TzTest ()
fund stkr from amount payload = Tz.transfer $
  Tz.TransferP
    { tpQty = amount
    , tpSrc = from
    , tpDst = stkr
    , tpBurnCap = 22
    , tpArgument = STKR.PublicEntrypoint (STKR.Fund payload)
    }

-- We dont' bother with getBalance/getTotalSupply entrypoints ATM, simply use
--   getStorage primitive, and return necessary values immediately.
-- Quick and dirty, we introduce no custom error type, supply error
--   messages directly.

withGetStorage :: (STKR.AlmostStorage -> TzTest ()) -> String -> Address -> TzTest ()
withGetStorage f partname stkr = do
  st@STKR.AlmostStorage{..} <- STKR.getStorage stkr
  if isNothing successor
    then f st
    else fail upgradedError
  where
    upgradedError =
        "STKR contract is upgraded, please use " ++ partname
         ++ " entrypoint instead of getStorage API."

getTotalSupply :: Address -> TzTest ()
getTotalSupply =
  withGetStorage
    (\STKR.AlmostStorage{..} -> putTextLn $ "Total supply: " <> show totalSupply)
      "getTotalSupply"

getBalance :: Address -> Address -> TzTest ()
getBalance stkr whose = withGetStorage f "getBalance" stkr
  where
    f STKR.AlmostStorage{..} = do
        balance <- Tz.getElementTextOfBigMapByAddress whose ledger
        putTextLn $ "Balance: " <> balance
