{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Term.STLC.Eval.BigStep (
    bigStepInput
  ) where

import Control.Lens (preview)
import Data.Constraint.Forall (ForallT)

import Component.Term.Eval.BigStep (BigStepRule(..), BigStepInput(..))

import Component.Term.STLC (AsSTLCTerm(..), WithSTLCTerm, app_)

-- |
eLamApp :: ( WithSTLCTerm tm ty
           , ForallT Monad tm
           )
        => (tm nTy nTm String -> Maybe (tm nTy nTm String))
        -> tm nTy nTm String
        -> Maybe (tm nTy nTm String)
eLamApp step tm = do
  (tm1, tm2) <- preview _TmApp tm
  tm1' <- step tm1
  tm2' <- step tm2
  (_, _, s) <- preview _TmLam tm1'
  return $ app_ tm2' s

-- |
bigStepInput :: ( WithSTLCTerm tm ty
                , ForallT Monad tm
                )
             => BigStepInput tm nTy nTm String
bigStepInput =
  BigStepInput
    [BigStepRecurse eLamApp]
