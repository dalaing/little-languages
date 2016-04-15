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
data PrettyTermRule tm =
    PrettyTermBase (tm -> Maybe Doc)                   -- ^
  | PrettyTermRecurse ((tm -> Doc) -> tm -> Maybe Doc) -- ^

-- |
fixPrettyTermRule :: (tm -> Doc)
                  -> PrettyTermRule tm
                  -> tm
                  -> Maybe Doc
fixPrettyTermRule _ (PrettyTermBase f) x =
  f x
fixPrettyTermRule step (PrettyTermRecurse f) x =
  f step x

-- |
data PrettyTermInput tm =
  PrettyTermInput [PrettyTermRule tm] -- ^

instance Monoid (PrettyTermInput tm) where
  mempty =
    PrettyTermInput mempty
  mappend (PrettyTermInput v1) (PrettyTermInput v2) =
    PrettyTermInput (mappend v1 v2)

-- |
data PrettyTermOutput tm =
  PrettyTermOutput {
    _prettyTerm      :: tm -> Doc         -- ^
  , _prettyTermRules :: [tm -> Maybe Doc] -- ^
  }

makeClassy ''PrettyTermOutput

-- |
mkPrettyTerm :: PrettyTermInput tm  -- ^
             -> PrettyTermOutput tm -- ^
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
