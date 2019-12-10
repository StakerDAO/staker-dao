module Test.Lorentz.Contracts.Multisig
  ( spec_Call
  , spec_RotateKeys
  ) where

import Prelude

import Data.Set (Set)
import qualified Data.Set as Set
import Fmt ((+|), (|+))
import Lens.Micro (ix, (&), (.~))
import Lorentz (( # ))
import qualified Lorentz as L
import Lorentz.Constraints (NicePackedValue, NiceParameter)
import Lorentz.Pack (lPackValue)
import Lorentz.Test
import Test.Hspec (Spec, it)
import Tezos.Core (dummyChainId, mkChainIdUnsafe)
import Tezos.Crypto (KeyHash, PublicKey, SecretKey, detSecretKey, hashKey, sign, toPublic)

import Lorentz.Contracts.Client (multisignValue)
import Lorentz.Contracts.Multisig

originate
  :: forall a. (NiceParameter a, NicePackedValue a)
  => [PublicKey]
  -> IntegrationalScenarioM (L.ContractRef (Parameter a))
originate teamKeys =
  lOriginate multisigContract "Operation team multisig"
    Storage
      { teamKeys = Set.fromList $ hashKey <$> teamKeys
      , currentNonce = 0
      }
    (L.toMutez 0)

-- | Keys of an attacker
evilSK :: SecretKey
evilSK = detSecretKey "eva"

evilPK :: PublicKey
evilPK = toPublic evilSK

-- | Deterministic secret keys
sk :: Int -> SecretKey
sk = detSecretKey . show

-- | Deterministic public keys
pk :: Int -> PublicKey
pk = toPublic . sk

-- | Team secret keys for 5-member team.
secretKeys :: [SecretKey]
secretKeys = sk <$> [1 .. 5]

-- | Team public keys for 5-member team.
publicKeys :: [PublicKey]
publicKeys = toPublic <$> secretKeys

-- | Public key hashes of the new team members.
newTeamPKHs :: Set KeyHash
newTeamPKHs = Set.fromList $ hashKey . pk <$> [1, 2, 3, 8, 9, 10]

-- | A contract that sets the flag to True when invoked. We use this
-- flag contract to make sure the transaction went through.
flagContract :: L.Contract () Bool
flagContract = L.drop # L.push True # L.nil # L.pair

-- | An utility function that signs the order and calls Multisig
-- with the correct parameter.
callMsig
  :: forall a. (NiceParameter a, NicePackedValue a)
  => L.ContractRef (Parameter a) -> L.ChainId -> Natural -> Order a
  -> [SecretKey] -> IntegrationalScenarioM ()
callMsig msig chainId nonce order teamSecretKeys = do
  let toSign = ValueToSign
            { vtsChainId = chainId
            , vtsNonce = nonce
            , vtsOrder = order
            }

  let signatures = multisignValue teamSecretKeys toSign
  lCall msig $
    Parameter
      { order = order
      , nonce = nonce
      , signatures = signatures
      }

spec_Call :: Spec
spec_Call = do
  it "makes a requested call if everything is correct (3 of 5 signatures)" $
    integrationalTestExpectation $ do
      msig <- originate publicKeys
      flag <- lOriginate flagContract "flag" False (L.toMutez 0)
      let order = mkCallOrder flag ()

      callMsig msig dummyChainId 1 order (take 3 secretKeys)
      validate . Right $ lExpectStorageConst flag True

  it "makes a requested call if everything is correct (3 of 4 signatures)" $
    integrationalTestExpectation $ do
      msig <- originate $ take 4 publicKeys
      flag <- lOriginate flagContract "flag" False (L.toMutez 0)
      let order = mkCallOrder flag ()

      callMsig msig dummyChainId 1 order (take 3 secretKeys)
      validate . Right $ lExpectStorageConst flag True

  it "fails if call parameter is of incorrect type" $
    integrationalTestExpectation $ do
      msig <- originate publicKeys
      flag <- lOriginate flagContract "flag" False (L.toMutez 0)
      let flagAddr = L.fromContractAddr flag
      let wrongOrder = mkCallOrderUnsafe flagAddr (777 :: Natural)

      callMsig msig dummyChainId 1 wrongOrder (take 3 secretKeys)
      validate . Left $
        lExpectCustomError #invalidStakerContract ()

  generalFailuresSpec $ do
    flag <- lOriginate flagContract "flag" False (L.toMutez 0)
    return $ mkCallOrder flag ()

spec_RotateKeys :: Spec
spec_RotateKeys = do
  it "updates keys if everything is correct (3 of 5 signatures)" $
    integrationalTestExpectation $ do
      let order = mkRotateKeysOrder newTeamPKHs
      msig <- originate @() publicKeys

      callMsig msig dummyChainId 1 order (take 3 secretKeys)
      validate . Right $ lExpectStorageConst msig Storage
        { teamKeys = newTeamPKHs
        , currentNonce = 1
        }

  it "updates keys if everything is correct (3 of 4 signatures)" $
    integrationalTestExpectation $ do
      let order = mkRotateKeysOrder newTeamPKHs
      msig <- originate @() $ take 4 publicKeys

      callMsig msig dummyChainId 1 order (take 3 secretKeys)
      validate . Right $ lExpectStorageConst msig Storage
        { teamKeys = newTeamPKHs
        , currentNonce = 1
        }

  generalFailuresSpec $ do
    return $ mkRotateKeysOrder newTeamPKHs

-- | Specification of general failures that can occur during contract
-- calls, i.e. invalid chain id, nonce, signature, and too little
-- amount of signers.
generalFailuresSpec :: IntegrationalScenarioM (Order ()) -> Spec
generalFailuresSpec mkOrder = do
  it "fails if chain id is incorrect" $
    integrationalTestExpectation $ do
      order <- mkOrder
      msig <- originate @() publicKeys
      let wrongChainId = mkChainIdUnsafe "1234"

      callMsig msig wrongChainId 1 order (take 3 secretKeys)
      validate . Left $
        lExpectCustomError #invalidSignature (pk 1)

  it "fails if nonce is incorrect" $
    integrationalTestExpectation $ do
      order <- mkOrder
      msig <- originate publicKeys
      let wrongNonce = 2

      callMsig msig dummyChainId wrongNonce order (take 3 secretKeys)
      validate . Left $
        lExpectCustomError #invalidNonce ()

  it "fails if majority quorum of team keys is not reached (2 of 5 signatures)" $
    integrationalTestExpectation $ do
      order <- mkOrder
      msig <- originate publicKeys
      let lessThanQuorumKeys = take 2 secretKeys

      callMsig msig dummyChainId 1 order lessThanQuorumKeys
      validate . Left $
        lExpectCustomError #majorityQuorumNotReached ()

  it "fails if majority quorum of team keys is not reached (2 of 4 signatures)" $
    integrationalTestExpectation $ do
      order <- mkOrder
      msig <- originate $ take 4 publicKeys
      let lessThanQuorumKeys = take 2 secretKeys

      callMsig msig dummyChainId 1 order lessThanQuorumKeys
      validate . Left $
        lExpectCustomError #majorityQuorumNotReached ()

  it "ignores keys supplied several times" $
    integrationalTestExpectation $ do
      order <- mkOrder
      msig <- originate $ take 4 publicKeys
      let sk1 = sk 1
      let multipleIdenticalKeys = [sk1, sk1, sk1] ++ take 1 secretKeys

      callMsig msig dummyChainId 1 order multipleIdenticalKeys
      validate . Left $
        lExpectCustomError #majorityQuorumNotReached ()

  it "fails if no signatures were provided" $
    integrationalTestExpectation $ do
      order <- mkOrder
      msig <- originate $ take 4 publicKeys

      callMsig msig dummyChainId 1 order []
      validate . Left $
        lExpectCustomError #majorityQuorumNotReached ()

  wrongKeySpec mkOrder [1, 3, 5]
  wrongSignatureSpec mkOrder [1, 3, 5]

-- | Tests keys that are not eligible at different positions
wrongKeySpec :: IntegrationalScenarioM (Order ()) -> [Int] -> Spec
wrongKeySpec mkOrder positions =
  foldl' (>>) (pure ()) $ positions <&> \pos ->
    it ("fails if one of the keys is not eligible (at pos " +| pos |+ ")") $
      integrationalTestExpectation $ do
        order <- mkOrder
        msig <- originate @() publicKeys
        let wrongKeys = secretKeys & ix (pos - 1) .~ evilSK

        callMsig msig dummyChainId 1 order wrongKeys
        validate . Left $
          lExpectCustomError #invalidSignature evilPK

-- | Tests invalid signatures provided at different positions
wrongSignatureSpec :: IntegrationalScenarioM (Order ()) -> [Int] -> Spec
wrongSignatureSpec mkOrder positions =
  foldl' (>>) (pure ()) $ positions <&> \pos ->
    it ("fails if one of the signatures is not correct (at pos " +| pos |+ ")") $
      integrationalTestExpectation $ do
        order <- mkOrder
        msig <- originate @() publicKeys
        let nonce = 1
        let toSign = ValueToSign
              { vtsChainId = dummyChainId
              , vtsNonce = nonce
              , vtsOrder = order
              }

        let correctSignatures = multisignValue secretKeys toSign
        let evilSignature = sign evilSK (lPackValue toSign)
        let messedSignatures = correctSignatures & ix (pos - 1) . _2 .~ evilSignature
        lCall msig $
          Parameter
            { order = order
            , nonce = nonce
            , signatures = messedSignatures
            }

        validate . Left $
          lExpectCustomError #invalidSignature $ pk pos
