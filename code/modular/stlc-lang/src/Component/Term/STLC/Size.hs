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
module Component.Term.STLC.Size (
    termSizeInput
  ) where

import Control.Lens (review, preview)
import Bound (instantiate1)

import Component.Term.Size (TermSizeRule(..), TermSizeInput(..))

import Component.Term.STLC (AsSTLCTerm(..), AsSTLCVar(..), WithSTLCTerm)

termSizeTmVar :: WithSTLCTerm tm ty tyN tmN a
              => tm tmN a
              -> Maybe Int
termSizeTmVar =
  fmap (const 1) .
  preview _TmVar

termSizeTmApp :: ( WithSTLCTerm tm ty nTy nTm a
                 , Monad (tm nTm)
                 )
              => (tm nTm a -> Int)
              -> tm nTm a
              -> Maybe Int
termSizeTmApp size =
    fmap termSizeTmApp' .
    preview _TmApp
  where
    termSizeTmApp' (tm1, tm2) =
      1 + size tm1 + size tm2

-- the solution is to go back to the usual WithSTLCTerm form, and to use
-- instantiate1 to get rid of the scope
termSizeTmLam :: ( WithSTLCTerm tm ty nTy nTm String
                 , Monad (tm nTm)
                 )
              => (tm nTm String -> Int)
              -> tm nTm String
              -> Maybe Int
termSizeTmLam size =
    fmap termSizeTmLam' .
    preview _TmLam
  where
    termSizeTmLam' (n, _, s) =
      1 + size (instantiate1 (review _TmVar n) s)

termSizeInput :: ( WithSTLCTerm tm ty nty nTm String
                 , Monad (tm nTm)
                 )
              => TermSizeInput tm nTm String
termSizeInput =
  TermSizeInput
    [ TermSizeBase termSizeTmVar
    , TermSizeRecurse termSizeTmLam
    , TermSizeRecurse termSizeTmApp
    ]
