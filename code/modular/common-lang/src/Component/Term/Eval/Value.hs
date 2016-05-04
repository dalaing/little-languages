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

import Component.Term.Note.Strip (StripNoteTerm(..))

-- |
data ValueRule tm nTy nTm a =
    ValueBase (tm nTy nTm a -> Maybe (tm nTy nTm a))                        -- ^
  | ValueRecurse ((tm nTy nTm a -> Maybe (tm nTy nTm a)) -> tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^

-- |
fixValueRule :: (tm nTy nTm a -> Maybe (tm nTy nTm a))
             -> ValueRule tm nTy nTm a
             -> tm nTy nTm a
             -> Maybe (tm nTy nTm a)
fixValueRule _ (ValueBase f) x =
  f x
fixValueRule step (ValueRecurse f) x =
  f step x

-- |
data ValueInput tm nTy nTm a =
  ValueInput [ValueRule tm nTy nTm a] -- ^

instance Monoid (ValueInput tm nTy nTm a) where
  mempty =
    ValueInput mempty
  mappend (ValueInput v1) (ValueInput v2) =
    ValueInput (mappend v1 v2)

-- |
data ValueOutput tm nTy nTm a =
  ValueOutput {
    _value      :: tm nTy nTm a -> Maybe (tm nTy nTm a)   -- ^
  , _valueRules :: [tm nTy nTm a -> Maybe (tm nTy nTm a)] -- ^
  , _isValue    :: tm nTy nTm a -> Bool -- ^
  }

makeClassy ''ValueOutput

-- |
mkValue :: StripNoteTerm tm tm
        => ValueInput tm nTy nTm a -- ^
        -> ValueOutput tm nTy nTm a -- ^
mkValue (ValueInput i) =
  let
    valueRules' =
      fmap (fixValueRule value') i
    value' tm =
      asum .
      fmap ($ stripNoteTerm tm) $
      valueRules'
  in
    ValueOutput
      value'
      valueRules'
      (isJust . value')
