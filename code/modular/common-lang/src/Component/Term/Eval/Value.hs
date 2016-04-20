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

import Component.Term.Note.Strip (StripNoteTermOutput(..))

-- |
data ValueRule tm n a =
    ValueBase (tm n a -> Maybe (tm n a))                        -- ^
  | ValueRecurse ((tm n a -> Maybe (tm n a)) -> tm n a -> Maybe (tm n a)) -- ^

-- |
fixValueRule :: (tm n a -> Maybe (tm n a))
             -> ValueRule tm n a
             -> tm n a
             -> Maybe (tm n a)
fixValueRule _ (ValueBase f) x =
  f x
fixValueRule step (ValueRecurse f) x =
  f step x

-- |
data ValueInput tm n a =
  ValueInput [ValueRule tm n a] -- ^

instance Monoid (ValueInput tm n a) where
  mempty =
    ValueInput mempty
  mappend (ValueInput v1) (ValueInput v2) =
    ValueInput (mappend v1 v2)

-- |
data ValueOutput tm n a =
  ValueOutput {
    _value      :: tm n a -> Maybe (tm n a)   -- ^
  , _valueRules :: [tm n a -> Maybe (tm n a)] -- ^
  , _isValue    :: tm n a -> Bool -- ^
  }

makeClassy ''ValueOutput

-- |
mkValue :: StripNoteTermOutput tm
        -> ValueInput tm n a -- ^
        -> ValueOutput tm n a -- ^
mkValue (StripNoteTermOutput _ stripNote) (ValueInput i) =
  let
    valueRules' =
      fmap (fixValueRule value') i
    value' tm =
      asum .
      fmap ($ stripNote tm) $
      valueRules'
  in
    ValueOutput
      value'
      valueRules'
      (isJust . value')
