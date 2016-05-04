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

import Component.Term.Note.Strip (StripNoteTerm(..))

-- |
data BigStepRule tm nTy nTm a =
    BigStepBase (tm nTy nTm a -> Maybe (tm nTy nTm a))                        -- ^
  | BigStepRecurse ((tm nTy nTm a -> Maybe (tm nTy nTm a)) -> tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^

-- |
fixBigStepRule :: (tm nTy nTm a -> Maybe (tm nTy nTm a))
             -> BigStepRule tm nTy nTm a
             -> tm nTy nTm a
             -> Maybe (tm nTy nTm a)
fixBigStepRule _ (BigStepBase f) x =
  f x
fixBigStepRule step (BigStepRecurse f) x =
  f step x

-- |
data BigStepInput tm nTy nTm a =
  BigStepInput [BigStepRule tm nTy nTm a] -- ^

instance Monoid (BigStepInput tm nTy nTm a) where
  mempty =
    BigStepInput mempty
  mappend (BigStepInput v1) (BigStepInput v2) =
    BigStepInput (mappend v1 v2)

-- |
data BigStepOutput tm nTy nTm a =
  BigStepOutput {
    _bigStep      :: tm nTy nTm a -> Maybe (tm nTy nTm a)   -- ^
  , _bigStepRules :: [tm nTy nTm a -> Maybe (tm nTy nTm a)] -- ^
  , _bigStepEval  :: tm nTy nTm a -> tm nTy nTm a        -- ^
  }

makeClassy ''BigStepOutput

-- |
mkBigStep :: StripNoteTerm tm tm
          => BigStepInput tm nTy nTm a -- ^
          -> BigStepOutput tm nTy nTm a-- ^
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
