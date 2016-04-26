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
import Bound (instantiate1)

import Component.Term.Eval.BigStep (BigStepRule(..), BigStepInput(..))

import Component.Term.STLC (AsSTLCTerm(..), WithSTLCTerm)

-- |
eLamApp :: ( WithSTLCTerm tm ty nTy nTm a
           , Monad (tm nTm)
           )
        => (tm nTm a -> Maybe (tm nTm a))
        -> tm nTm a
        -> Maybe (tm nTm a)
eLamApp step tm = do
  (tm1, tm2) <- preview _TmApp tm
  tm1' <- step tm1
  tm2' <- step tm2
  (_, _, s) <- preview _TmLam tm1'
  return $ instantiate1 tm2' s

-- |
bigStepInput :: ( WithSTLCTerm tm ty nTy nTm a
                , Monad (tm nTm)
                )
             => BigStepInput tm nTm a
bigStepInput =
  BigStepInput
    [BigStepRecurse eLamApp]
