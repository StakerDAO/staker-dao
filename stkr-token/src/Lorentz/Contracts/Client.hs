module Lorentz.Contracts.Client
  ( DeployOptions (..)
  , deploy
  , multisignBytes
  , multisignValue
  , signBytes
  , ContractAddresses (..)
  , ViaMultisigOptions (..)
  , callViaMultisig
  , VoteForProposalOptions (..)
  , voteForProposal
  ) where

import Prelude

import Fmt (Buildable(..), Builder, mapF)
import Named (arg)

import Lorentz.Constraints (NicePackedValue)
import Lorentz.Pack (lPackValue)
import Tezos.Address (Address)
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

callViaMultisig
  :: Msig.TransferOrderWrapC STKR.Parameter cName it
  => Msig.Label cName -> it -> ViaMultisigOptions -> TzTest ()
callViaMultisig label stkrParam ViaMultisigOptions {..} = do
  let order = Msig.mkCallOrderWrap @STKR.Parameter (Msig.Unsafe vmoStkr) label stkrParam
  chainId <- Tz.getMainChainId
  let getNonce = (+1) . Msig.currentNonce <$> Tz.getStorage vmoMsig
  nonce <- maybe getNonce pure vmoNonce
  let toSign = Msig.ValueToSign chainId nonce order
  let bytes = lPackValue toSign
  pkSigs <- vmoSign bytes
  let param = Msig.Parameter order nonce pkSigs
  Tz.call vmoFrom vmoMsig param

data ViaMultisigOptions = ViaMultisigOptions
  { vmoMsig :: Address
  , vmoStkr :: Address
  , vmoFrom :: Address
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

voteForProposal :: VoteForProposalOptions -> TzTest ()
voteForProposal VoteForProposalOptions {..} = do
  STKR.AlmostStorage{..} <- STKR.getStorage vpStkr
  proposalHash <-
    maybe (fail $ "Proposal id not found " <> show vpProposalId) pure .
    fmap (arg #proposalHash . snd) . safeHead . snd . splitAt (fromIntegral vpProposalId - 1) $ proposals
  let curStage = vpEpoch*4 + 2
  let toSignB = lPackValue $ STKR.CouncilDataToSign proposalHash vpStkr curStage
  (pk, sig) <- vpSign toSignB
  Tz.call vpFrom vpStkr $ STKR.VoteForProposal
    (#proposalId vpProposalId, #votePk pk, #voteSig sig)
