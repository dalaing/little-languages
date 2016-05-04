{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Component.Term.Eval.BigStep (
    BigStepRule(..)
  , BigStepInput(..)
  , BigStepOutput(..)
  , HasBigStepOutput(..)
  , mkBigStep
  ) where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

import Control.Lens.TH (makeClassy)

import Component.Term.Note.Strip (StripNoteTerm(..))

-- |
data BigStepRule tm =
    BigStepBase (forall nTy nTm. tm nTy nTm String -> Maybe (tm nTy nTm String))                        -- ^
  | BigStepRecurse (forall nTy nTm. (tm nTy nTm String -> Maybe (tm nTy nTm String)) -> tm nTy nTm String -> Maybe (tm nTy nTm String)) -- ^

-- |
fixBigStepRule :: (tm nTy nTm String -> Maybe (tm nTy nTm String))
             -> BigStepRule tm
             -> tm nTy nTm String
             -> Maybe (tm nTy nTm String)
fixBigStepRule _ (BigStepBase f) x =
  f x
fixBigStepRule step (BigStepRecurse f) x =
  f step x

-- |
data BigStepInput tm =
  BigStepInput [BigStepRule tm] -- ^

instance Monoid (BigStepInput tm) where
  mempty =
    BigStepInput mempty
  mappend (BigStepInput v1) (BigStepInput v2) =
    BigStepInput (mappend v1 v2)

-- |
data BigStepOutput tm =
  BigStepOutput {
    _bigStep      :: forall nTy nTm. tm nTy nTm String -> Maybe (tm nTy nTm String)   -- ^
  , _bigStepRules :: forall nTy nTm. [tm nTy nTm String -> Maybe (tm nTy nTm String)] -- ^
  , _bigStepEval  :: forall nTy nTm. tm nTy nTm String -> tm nTy nTm String        -- ^
  }

makeClassy ''BigStepOutput

-- |
mkBigStep :: StripNoteTerm tm tm
          => BigStepInput tm -- ^
          -> BigStepOutput tm -- ^
mkBigStep (BigStepInput i) =
  let
    bigStepRules' =
      fmap (fixBigStepRule bigStep') i
    bigStep' tm =
      asum .
      fmap ($ stripNoteTerm tm) $
      bigStepRules'
    bigStepEval' tm =
      fromMaybe (stripNoteTerm tm) .
      bigStep' $
      tm
  in
    BigStepOutput
      bigStep'
      bigStepRules'
      bigStepEval'
