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
data BigStepRule tm n a =
    BigStepBase (tm n a -> Maybe (tm n a))                        -- ^
  | BigStepRecurse ((tm n a -> Maybe (tm n a)) -> tm n a -> Maybe (tm n a)) -- ^

-- |
fixBigStepRule :: (tm n a -> Maybe (tm n a))
             -> BigStepRule tm n a
             -> tm n a
             -> Maybe (tm n a)
fixBigStepRule _ (BigStepBase f) x =
  f x
fixBigStepRule step (BigStepRecurse f) x =
  f step x

-- |
data BigStepInput tm n a =
  BigStepInput [BigStepRule tm n a] -- ^

instance Monoid (BigStepInput tm n a) where
  mempty =
    BigStepInput mempty
  mappend (BigStepInput v1) (BigStepInput v2) =
    BigStepInput (mappend v1 v2)

-- |
data BigStepOutput tm n a =
  BigStepOutput {
    _bigStep      :: tm n a -> Maybe (tm n a)   -- ^
  , _bigStepRules :: [tm n a -> Maybe (tm n a)] -- ^
  , _bigStepEval  :: tm n a -> tm n a        -- ^
  }

makeClassy ''BigStepOutput

-- |
mkBigStep :: StripNoteTerm tm tm
          => BigStepInput tm n a -- ^
          -> BigStepOutput tm n a-- ^
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
