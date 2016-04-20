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
module Component.Term.Pretty (
    PrettyTermRule(..)
  , PrettyTermInput(..)
  , PrettyTermOutput
  , HasPrettyTermOutput(..)
  , mkPrettyTerm
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Control.Lens.TH (makeClassy)
import Text.PrettyPrint.ANSI.Leijen (Doc, text)

-- |
data PrettyTermRule tm n a =
    PrettyTermBase (tm n a -> Maybe Doc)                   -- ^
  | PrettyTermRecurse ((tm n a -> Doc) -> tm n a -> Maybe Doc) -- ^

-- |
fixPrettyTermRule :: (tm n a -> Doc)
                  -> PrettyTermRule tm n a
                  -> tm n a
                  -> Maybe Doc
fixPrettyTermRule _ (PrettyTermBase f) x =
  f x
fixPrettyTermRule step (PrettyTermRecurse f) x =
  f step x

-- |
data PrettyTermInput tm n a =
  PrettyTermInput [PrettyTermRule tm n a] -- ^

instance Monoid (PrettyTermInput tm n a) where
  mempty =
    PrettyTermInput mempty
  mappend (PrettyTermInput v1) (PrettyTermInput v2) =
    PrettyTermInput (mappend v1 v2)

-- |
data PrettyTermOutput tm n a =
  PrettyTermOutput {
    _prettyTerm      :: tm n a -> Doc         -- ^
  , _prettyTermRules :: [tm n a -> Maybe Doc] -- ^
  }

makeClassy ''PrettyTermOutput

-- |
mkPrettyTerm :: PrettyTermInput tm n a  -- ^
             -> PrettyTermOutput tm n a -- ^
mkPrettyTerm (PrettyTermInput i) =
  let
    prettyTermRules' =
      fmap (fixPrettyTermRule prettyTerm') i
    prettyTerm' tm =
      fromMaybe (text "???") .
      asum .
      fmap ($ tm) $
      prettyTermRules'
  in
    PrettyTermOutput
      prettyTerm'
      prettyTermRules'
