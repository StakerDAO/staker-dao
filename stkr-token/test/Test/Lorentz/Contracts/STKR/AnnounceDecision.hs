module Test.Lorentz.Contracts.STKR.AnnounceDecision
  ( spec_AnnounceDecision
  ) where

import Prelude

import Data.Map.Strict as Map
import Lens.Micro
import Test.Hspec (Spec, it)

import Lorentz.Test
import Lorentz.Pack (lPackValue)
import Michelson.Text
import Util.Named ((.!))
import Tezos.Crypto (sign)

import qualified Lorentz.Contracts.STKR as STKR
import Lorentz.Contracts.Client (multisignValue)

import Test.Lorentz.Contracts.STKR.Common (newKeypair, originate)

spec_AnnounceDecision :: Spec
spec_AnnounceDecision = announceDecisionSpec

announceDecisionSpec :: Spec
announceDecisionSpec = do
  let (sk1, pk1) = newKeypair "1"
  let (sk2, pk2) = newKeypair "2"
  let (sk3, pk3) = newKeypair "3"
  let (sk4, pk4) = newKeypair "4"
  let (sk5, pk5) = newKeypair "5"
  let (sk6, _) = newKeypair "6"
  let newUrls = Map.fromList $
        [ ([mt|resource1|], ("hash1", [mt|https://example.com/resource1|]))
        , ([mt|resource2|], ("hash2", [mt|https://example.com/resource2|]))
        ] :: Map MText (ByteString, MText)
  it "update urls if quorum is reached" $
    integrationalTestExpectation $ do
      let councilPks = [pk1, pk2, pk3, pk4, pk5]
      let councilSks = [sk1, sk2, sk3, sk4, sk5]
      (_, stkr) <- originate [] councilPks
      lCall stkr $
        STKR.AnnounceDecision
          ( #description .! [mt|"Oh, what a description!"|]
          , #approvals .! multisignValue councilSks newUrls
          , #newUrls .! newUrls
          )

      validate . Right . lExpectStorageUpdate stkr $ \storage ->
        if newUrls == (STKR.urls storage)
        then pass
        else Left . CustomValidationError $
                "Invalid URLs"

  it "fail if one of signatures in approvals is incorrect" $
    integrationalTestExpectation $ do
      let councilPks = [pk1, pk2, pk3, pk4, pk5]
      let councilSks = [sk1, sk2, sk3, sk4, sk5]
      let correctlySignedKeys = multisignValue councilSks newUrls
      let wrongSignature = sign sk6 (lPackValue newUrls)
      let messedKeys = correctlySignedKeys & ix 3 . _2 .~ wrongSignature
      (_, stkr) <- originate [] councilPks
      lCall stkr $
        STKR.AnnounceDecision
          ( #description .! [mt|"Oh, what a description!"|]
          , #approvals .! messedKeys
          , #newUrls .! newUrls
          )

      validate . Left $
        lExpectCustomError #invalidSignature pk4

  it "fail if majority quorum of council keys is not reached" $
    integrationalTestExpectation $ do
      let councilPks = [pk1, pk2, pk3, pk4, pk5]
      let correctlySignedKeys = multisignValue [sk2, sk4] newUrls
      (_, stkr) <- originate [] councilPks
      lCall stkr $
        STKR.AnnounceDecision
          ( #description .! [mt|"Oh, what a description!"|]
          , #approvals .! correctlySignedKeys
          , #newUrls .! newUrls
          )
      validate . Left $
        lExpectCustomError #majorityQuorumNotReached ()
