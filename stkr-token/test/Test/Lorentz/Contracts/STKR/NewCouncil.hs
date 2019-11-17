module Test.Lorentz.Contracts.STKR.NewCouncil
  ( spec_NewCouncil
  ) where

import Prelude

import Lens.Micro
import Lorentz.Pack
import Lorentz.Test
import Test.Hspec (Spec, it)
import Tezos.Crypto (sign)
import Util.Named ((.!))

import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common (multisignValue, newKeypair, originate)

spec_NewCouncil :: Spec
spec_NewCouncil = newCouncilSpec

newCouncilSpec :: Spec
newCouncilSpec = do
  let (sk1, pk1) = newKeypair "1"
  let (sk2, pk2) = newKeypair "2"
  let (sk3, pk3) = newKeypair "3"
  let (sk4, pk4) = newKeypair "4"
  let (sk5, pk5) = newKeypair "5"
  let (sk6, pk6) = newKeypair "6"
  let (_, pk7) = newKeypair "7"
  it "update council keys if quorum is reached" $
    integrationalTestExpectation $ do
      let teamPks = [pk1, pk2, pk3, pk4, pk5]
      let teamSks = [sk1, sk2, sk3, sk4, sk5]
      let newCouncilKeys = [pk6, pk7]
      stkr <- originate $
        STKR.Storage
          { teamKeys = teamPks
          , councilKeys = []
          , urls = mempty
          }
      lCall stkr $
        STKR.NewConcuil
          ( #epochId .! 0
          , #approvals .! multisignValue teamSks newCouncilKeys
          , #newCouncil .! newCouncilKeys
          )
      validate . Right . lExpectStorageConst stkr $
        STKR.Storage
          { teamKeys = teamPks
          , councilKeys = newCouncilKeys
          , urls = mempty
          }

  it "fail if one of signature in approvals is incorrect" $
    integrationalTestExpectation $ do
      let teamPks = [pk1, pk2, pk3, pk4, pk5]
      let teamSks = [sk1, sk2, sk3, sk4, sk5]
      let newCouncilKeys = [pk6, pk7]
      let correctlySignedKeys = multisignValue teamSks newCouncilKeys
      let wrongSignature = sign sk6 (lPackValue newCouncilKeys)
      let messedKeys = correctlySignedKeys & ix 3 . _2 .~ wrongSignature
      stkr <- originate $
        STKR.Storage
          { teamKeys = teamPks
          , councilKeys = []
          , urls = mempty
          }
      lCall stkr $
        STKR.NewConcuil
          ( #epochId .! 0
          , #approvals .! messedKeys
          , #newCouncil .! newCouncilKeys
          )
      validate . Left $
        lExpectCustomError #invalidSignature pk4

  it "fail if majority quorum of team keys is not reached" $
    integrationalTestExpectation $ do
      let teamPks = [pk1, pk2, pk3, pk4, pk5]
      let newCouncilKeys = [pk6, pk7]
      let correctlySignedKeys = multisignValue [sk2, sk4] newCouncilKeys
      stkr <- originate $
        STKR.Storage
          { teamKeys = teamPks
          , councilKeys = []
          , urls = mempty
          }
      lCall stkr $
        STKR.NewConcuil
          ( #epochId .! 0
          , #approvals .! correctlySignedKeys
          , #newCouncil .! newCouncilKeys
          )
      validate . Left $
        lExpectCustomError #majorityQuorumNotReached ()
