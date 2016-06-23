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
import Data.Constraint.Forall (ForallT)

import Component.Term.SubTerm (SubTermRule(..), SubTermInput(..))

import Component.Term.STLC (AsSTLCTerm(..), AsSTLCVar(..), WithSTLCTerm, app_)

subTermTmVar :: WithSTLCTerm tm ty
             => tm nTy nTm a
             -> Maybe [tm nTy nTm a]
subTermTmVar =
  fmap (pure . review _TmVar) .
  preview _TmVar

subTermTmApp :: WithSTLCTerm tm ty
             => (tm nTy nTm a -> [tm nTy nTm a])
             -> tm nTy nTm a
             -> Maybe [tm nTy nTm a]
subTermTmApp subTerms tm =
    fmap subTermTmApp' .
    preview _TmApp $
    tm
  where
    subTermTmApp' (tm1, tm2) =
      tm : subTerms tm1 ++ subTerms tm2

-- the solution is to go back to the usual WithSTLCTerm form, and to use
-- instantiate1 to get rid of the scope
subTermTmLam :: ( WithSTLCTerm tm ty
                , ForallT Monad tm
                )
             => (tm nTy nTm String -> [tm nTy nTm String])
             -> tm nTy nTm String
             -> Maybe [tm nTy nTm String]
subTermTmLam subTerms tm =
    fmap subTermTmLam' .
    preview _TmLam $
    tm
  where
    subTermTmLam' (n, _, s) =
      tm : subTerms (app_ (review _TmVar n) s)

subTermInput :: ( WithSTLCTerm tm ty
                , ForallT Monad tm
                )
              => SubTermInput tm
subTermInput =
  SubTermInput
    [ SubTermBase subTermTmVar
    , SubTermRecurse subTermTmLam
    , SubTermRecurse subTermTmApp
    ]
