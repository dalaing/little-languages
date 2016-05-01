{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Nat.SubTerm (
    subTermInput
  ) where

import Control.Lens (preview, review)

import Component.Term.SubTerm (SubTermInput(..), SubTermRule(..))

import Component.Term.Nat (AsNatTerm(..), WithNatTerm)

subTermTmZero :: WithNatTerm tm
              => tm n a
              -> Maybe [tm n a]
subTermTmZero =
  fmap (pure . review _TmZero) .
  preview _TmZero

subTermTmSucc :: WithNatTerm tm
              => (tm n a -> [tm n a])
              -> tm n a
              -> Maybe [tm n a]
subTermTmSucc subTerms tm =
  fmap ((tm :) . subTerms) .
  preview _TmSucc $
  tm

subTermTmPred :: WithNatTerm tm
              => (tm n a -> [tm n a])
              -> tm n a
              -> Maybe [tm n a]
subTermTmPred subTerms tm =
  fmap ((tm :) . subTerms) .
  preview _TmPred $
  tm

subTermInput :: WithNatTerm tm
             => SubTermInput tm n a
subTermInput =
  SubTermInput
    [ SubTermBase subTermTmZero
    , SubTermRecurse subTermTmSucc
    , SubTermRecurse subTermTmPred
    ]
