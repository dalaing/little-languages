{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Term.Eval.Value (
    ValueInput(..)
  , HasValueInput(..)
  , ValueOutput(..)
  , HasValueOutput(..)
  , mkValue
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Lens.TH (makeClassy)

import Common.Recursion

data ValueInput tm =
  ValueInput {
    _valueSteps :: [Step (ReaderT tm Maybe tm)]
  }

makeClassy ''ValueInput

instance Monoid (ValueInput tm) where
  mempty =
    ValueInput mempty
  mappend (ValueInput v1) (ValueInput v2) =
    ValueInput (mappend v1 v2)

data ValueOutput tm =
  ValueOutput {
    _value :: tm -> Maybe tm
  , _valueRules :: [tm -> Maybe tm]
  }

makeClassy ''ValueOutput

mkValue :: ValueInput tm
        -> ValueOutput tm
mkValue (ValueInput i) =
  let
    vs = runReaderT $ combineSteps i
  in
    ValueOutput
      vs
      (runReaderT <$> mkSteps i)
