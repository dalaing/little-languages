{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Component.Term.STLC.SubTerm (
    subTermInput
  ) where

import Control.Lens (review, preview)
import Bound (instantiate1)

import Component.Term.SubTerm (SubTermRule(..), SubTermInput(..))

import Component.Term.STLC (AsSTLCTerm(..), AsSTLCVar(..), WithSTLCTerm)

subTermTmVar :: WithSTLCTerm tm ty tyN
             => tm tmN a
             -> Maybe [tm tmN a]
subTermTmVar =
  fmap (pure . review _TmVar) .
  preview _TmVar

subTermTmApp :: ( WithSTLCTerm tm ty nTy
                , Monad (tm nTm)
                )
             => (tm nTm a -> [tm nTm a])
             -> tm nTm a
             -> Maybe [tm nTm a]
subTermTmApp subTerms tm =
    fmap subTermTmApp' .
    preview _TmApp $
    tm
  where
    subTermTmApp' (tm1, tm2) =
      tm : subTerms tm1 ++ subTerms tm2

-- the solution is to go back to the usual WithSTLCTerm form, and to use
-- instantiate1 to get rid of the scope
subTermTmLam :: ( WithSTLCTerm tm ty nTy
                , Monad (tm nTm)
                )
             => (tm nTm String -> [tm nTm String])
             -> tm nTm String
             -> Maybe [tm nTm String]
subTermTmLam subTerms tm =
    fmap subTermTmLam' .
    preview _TmLam $
    tm
  where
    subTermTmLam' (n, _, s) =
      tm : subTerms (instantiate1 (review _TmVar n) s)

subTermInput :: ( WithSTLCTerm tm ty nty
                 , Monad (tm nTm)
                 )
              => SubTermInput tm nTm String
subTermInput =
  SubTermInput
    [ SubTermBase subTermTmVar
    , SubTermRecurse subTermTmLam
    , SubTermRecurse subTermTmApp
    ]
