{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Term.Eval.SmallStep (
    SmallStepInput(..)
  , HasSmallStepInput(..)
  , SmallStepOutput(..)
  , HasSmallStepOutput(..)
  , mkSmallStep
  ) where

import Control.Lens.TH (makeClassy)
import Control.Monad.Reader

import Common.Recursion

data SmallStepInput tm =
  SmallStepInput {
    _smallSteps :: [Step (ReaderT tm Maybe tm)]
  }

makeClassy ''SmallStepInput

instance Monoid (SmallStepInput tm) where
  mempty =
    SmallStepInput mempty
  mappend (SmallStepInput s1) (SmallStepInput s2) =
    SmallStepInput (mappend s1 s2)

data SmallStepOutput tm =
  SmallStepOutput {
    _smallStepRules :: [tm -> Maybe tm]
  , _smallStep :: tm -> Maybe tm
  , _smallStepEval :: tm -> tm
  }

makeClassy ''SmallStepOutput

mkSmallStep :: SmallStepInput tm
            -> SmallStepOutput tm
mkSmallStep (SmallStepInput i) =
  let
    ss = runReaderT $ combineSteps i
  in
  SmallStepOutput
    (runReaderT <$> mkSteps i)
    ss
    (mkSmallStepEval ss)

mkSmallStepEval :: (tm -> Maybe tm)
                  -> tm
                  -> tm
mkSmallStepEval step =
    eval
  where
    eval s = case step s of
      Just s' -> eval s'
      Nothing -> s
