module Lorentz.Contracts.STKR.Proposal
  ( ActiveTokenCode
  , PassiveTokenCode
  , Fee
  , PropId
  , BookState(..)
  , Proposal(..)
  ) where

import Lorentz

import Lorentz.Contracts.STKR.Types (Hash, URL, Rational)

type ActiveTokenCode = MText
type PassiveTokenCode = MText

-- FIXME! Create smart constructor checking if it's in [0 .. 1) (or [0 .. 1]???)
type Fee = Rational

type PropId = MText

type BookState =
  ( "assets" :! Maybe (Map ActiveTokenCode [(Address {- wallets -}, Rational)])
  , "fee" :! Maybe Fee
  , "liabilities" :! Maybe (Map PassiveTokenCode [(Address {- contracts -}, Rational)])
  )

-- Don't know what numerical type to use, choose the (al)most general
type Proposal =
  ( "propId" :! PropId -- ??? Some unique??? Need a monad to genereta them then ...
  , "description" :! MText
    -- list of wallets with amount of tokens in each
    -- in principle these may be diffs, not absolute vals
  , "newBookState" :! BookState
  , "newUrlMetaMap" :! Maybe (Map URL (Hash, URL)) -- -what's it?
  )
