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

-- |
data BigStepRule tm =
    BigStepBase (tm -> Maybe tm)                        -- ^
  | BigStepRecurse ((tm -> Maybe tm) -> tm -> Maybe tm) -- ^

-- |
fixBigStepRule :: (tm -> Maybe tm)
             -> BigStepRule tm
             -> tm
             -> Maybe tm
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
    _bigStep      :: tm -> Maybe tm   -- ^
  , _bigStepRules :: [tm -> Maybe tm] -- ^
  , _bigStepEval  :: tm -> tm         -- ^
  }

makeClassy ''BigStepOutput

-- |
mkBigStep :: BigStepInput tm  -- ^
          -> BigStepOutput tm -- ^
mkBigStep (BigStepInput i) =
  let
    bigStepRules' =
      fmap (fixBigStepRule bigStep') i
    bigStep' tm =
      asum .
      fmap ($ tm) $
      bigStepRules'
    bigStepEval' tm =
      fromMaybe tm .
      bigStep' $
      tm
  in
    BigStepOutput
      bigStep'
      bigStepRules'
      bigStepEval'
