module Test.Lorentz.Contracts.STKR.Governance.NewProposal
  ( spec_NewProposal
  ) where

import Prelude

import qualified Data.Map as Map
import Fmt (mapF, (+|), (|+))
import Lorentz (MText, lPackValue, mt)
import Lorentz.Test
import Named (arg)
import Test.Hspec (Spec, it)
import Tezos.Crypto (blake2b)
import Util.Named ((.!))

import qualified Lorentz.Contracts.STKR as STKR

import Test.Lorentz.Contracts.STKR.Common
  (callWithMultisig, failWhenNot, newKeypair, originateWithEmptyLedger)

getProposals :: STKR.Storage -> [(STKR.ProposalId, STKR.Proposal)]
getProposals storage = extractProposal <$> (Map.assocs $ STKR.proposals storage)
    where
      extractProposal
        :: (STKR.ProposalId, STKR.ProposalAndHash)
        -> (STKR.ProposalId, STKR.Proposal)
      extractProposal = fmap $ arg #proposal . fst

mkDummyProposal :: MText -> MText -> ByteString -> MText -> STKR.Proposal
mkDummyProposal description linkName linkHash url =
  (#description .! description, #newPolicy .! policy)
  where
    policy = (#urls .! Map.fromList [annotatedUrl])
    annotatedUrl = (linkName, (linkHash, url))

dummyProposal :: STKR.Proposal
dummyProposal =
  mkDummyProposal [mt|Hello|] [mt|sdao|] "000" [mt|https://stakerdao.dummy|]

dummyProposal2 :: STKR.Proposal
dummyProposal2 =
  mkDummyProposal [mt|Bye|] [mt|blend|] "002" [mt|https://stakerdao.yummy|]

spec_NewProposal :: Spec
spec_NewProposal = do
  let (sk1, pk1) = newKeypair "1"
  let (sk2, pk2) = newKeypair "2"
  let (sk3, pk3) = newKeypair "3"
  let (sk4, pk4) = newKeypair "4"
  let (sk5, pk5) = newKeypair "5"
  let teamPks = [pk1, pk2, pk3, pk4, pk5]
  let teamSks = [sk1, sk2, sk3, sk4, sk5]

  it "successfully injects new proposal" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []

      callWithMultisig msig 1 teamSks stkr $ STKR.NewProposal dummyProposal

      validate . Right . lExpectStorageUpdate stkr $ \storage ->
        failWhenNot (getProposals storage == [(1, dummyProposal)]) $
            "Expected [" +| dummyProposal |+ "] \
            \but got " +| mapF (getProposals storage) |+ ""

  it "successfully injects two distinct proposal" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []

      callWithMultisig msig 1 teamSks stkr $ STKR.NewProposal dummyProposal
      callWithMultisig msig 2 teamSks stkr $ STKR.NewProposal dummyProposal2
      let expectedProposals =
            [ (1, dummyProposal)
            , (2, dummyProposal2)
            ]

      validate . Right . lExpectStorageUpdate stkr $ \storage ->
        failWhenNot (getProposals storage == expectedProposals) $
            "Expected " +| mapF expectedProposals |+
            " but got " +| mapF (getProposals storage) |+ ""

  it "fails to inject a duplicate proposal" $
    integrationalTestExpectation $ do
      (msig, stkr) <- originateWithEmptyLedger teamPks []

      callWithMultisig msig 1 teamSks stkr $ STKR.NewProposal dummyProposal
      callWithMultisig msig 2 teamSks stkr $ STKR.NewProposal dummyProposal

      let expectedHash = STKR.Blake2BHash (blake2b . lPackValue $ dummyProposal)
      validate . Left $
        lExpectCustomError #duplicateProposal (#proposalHash .! expectedHash)
