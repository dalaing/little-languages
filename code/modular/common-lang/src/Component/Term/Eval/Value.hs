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
module Component.Term.Eval.Value (
    ValueRule(..)
  , ValueInput(..)
  , ValueOutput(..)
  , HasValueOutput(..)
  , mkValue
  ) where

import Data.Maybe (isJust)
import Data.Foldable (asum)

import Control.Lens.TH (makeClassy)

-- |
data ValueRule tm =
    ValueBase (tm -> Maybe tm)                        -- ^
  | ValueRecurse ((tm -> Maybe tm) -> tm -> Maybe tm) -- ^

-- |
fixValueRule :: (tm -> Maybe tm)
             -> ValueRule tm
             -> tm
             -> Maybe tm
fixValueRule _ (ValueBase f) x =
  f x
fixValueRule step (ValueRecurse f) x =
  f step x

-- |
data ValueInput tm =
  ValueInput [ValueRule tm] -- ^

instance Monoid (ValueInput tm) where
  mempty =
    ValueInput mempty
  mappend (ValueInput v1) (ValueInput v2) =
    ValueInput (mappend v1 v2)

-- |
data ValueOutput tm =
  ValueOutput {
    _value      :: tm -> Maybe tm   -- ^
  , _valueRules :: [tm -> Maybe tm] -- ^
  , _isValue    :: tm -> Bool -- ^
  }

makeClassy ''ValueOutput

-- |
mkValue :: ValueInput tm  -- ^
        -> ValueOutput tm -- ^
mkValue (ValueInput i) =
  let
    valueRules' =
      fmap (fixValueRule value') i
    value' tm =
      asum .
      fmap ($ tm) $
      valueRules'
  in
    ValueOutput
      value'
      valueRules'
      (isJust . value')
