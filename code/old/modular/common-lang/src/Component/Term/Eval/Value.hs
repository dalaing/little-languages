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
data ValueRule tm =
    ValueBase (forall nTy nTm. tm nTy nTm String -> Maybe (tm nTy nTm String))                        -- ^
  | ValueRecurse (forall nTy nTm. (tm nTy nTm String -> Maybe (tm nTy nTm String)) -> tm nTy nTm String -> Maybe (tm nTy nTm String)) -- ^

-- |
fixValueRule :: (tm nTy nTm String -> Maybe (tm nTy nTm String))
             -> ValueRule tm
             -> tm nTy nTm String
             -> Maybe (tm nTy nTm String)
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
    _value      :: forall nTy nTm. tm nTy nTm String -> Maybe (tm nTy nTm String)   -- ^
  , _valueRules :: forall nTy nTm. [tm nTy nTm String -> Maybe (tm nTy nTm String)] -- ^
  , _isValue    :: forall nTy nTm. tm nTy nTm String -> Bool -- ^
  }

makeClassy ''ValueOutput

-- |
mkValue :: StripNoteTerm tm tm
        => ValueInput tm -- ^
        -> ValueOutput tm -- ^
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
