{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Term.Eval.BigStep (
    BigStepInput(..)
  , HasBigStepInput(..)
  , BigStepOutput(..)
  , HasBigStepOutput(..)
  , mkBigStep
  ) where

import Control.Lens.TH (makeClassy)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)

import Common.Recursion

data BigStepInput tm =
  BigStepInput {
    _bigSteps :: [Step (ReaderT tm Maybe tm)]
  }

makeClassy ''BigStepInput

instance Monoid (BigStepInput tm) where
  mempty =
    BigStepInput mempty
  mappend (BigStepInput s1) (BigStepInput s2) =
    BigStepInput (mappend s1 s2)

data BigStepOutput tm =
  BigStepOutput {
    _bigStepRules :: [tm -> Maybe tm]
  , _bigStep :: tm -> Maybe tm
  , _bigStepEval :: tm -> tm
  }

makeClassy ''BigStepOutput

mkBigStep :: BigStepInput tm
            -> BigStepOutput tm
mkBigStep (BigStepInput i) =
  let
    bs = runReaderT $ combineSteps i
  in
  BigStepOutput
    (runReaderT <$> mkSteps i)
    bs
    (mkBigStepEval bs)

mkBigStepEval :: (tm -> Maybe tm)
                  -> tm
                  -> tm
mkBigStepEval step =
    eval
  where
    eval s =
      fromMaybe s .
      step $
      s
