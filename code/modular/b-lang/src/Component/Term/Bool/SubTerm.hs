{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Bool.SubTerm (
    subTermInput
  ) where

import Control.Lens (preview, review)

import Component.Term.SubTerm (SubTermInput(..), SubTermRule(..))

import Component.Term.Bool (AsBoolTerm(..), WithBoolTerm)

subTermTmFalse :: WithBoolTerm tm
               => tm nTy nTm a
               -> Maybe [tm nTy nTm a]
subTermTmFalse =
  fmap (pure . review _TmFalse) .
  preview _TmFalse

subTermTmTrue :: WithBoolTerm tm
              => tm nTy nTm a
              -> Maybe [tm nTy nTm a]
subTermTmTrue =
  fmap (pure . review _TmTrue) .
  preview _TmTrue

subTermTmIf :: WithBoolTerm tm
            => (tm nTy nTm a -> [tm nTy nTm a])
            -> tm nTy nTm a
            -> Maybe [tm nTy nTm a]
subTermTmIf subTerms tm =
    fmap subTermTmIf' .
    preview _TmIf $
    tm
  where
    subTermTmIf' (tm1, tm2, tm3) =
      tm : subTerms tm1 ++ subTerms tm2 ++ subTerms tm3

subTermInput :: WithBoolTerm tm
             => SubTermInput tm nTy nTm a
subTermInput =
  SubTermInput
    [ SubTermBase subTermTmFalse
    , SubTermBase subTermTmTrue
    , SubTermRecurse subTermTmIf
    ]
