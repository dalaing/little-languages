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
module Component.Type.Error.Pretty (
    PrettyTypeErrorRule(..)
  , PrettyTypeErrorInput(..)
  , PrettyTypeErrorOutput(..)
  , HasPrettyTypeErrorOutput(..)
  , mkPrettyTypeError
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Control.Lens.TH (makeClassy)
import Text.PrettyPrint.ANSI.Leijen (Doc, text)

import Component.Type.Pretty (PrettyTypeOutput(..))

-- |
data PrettyTypeErrorRule e ty nTy =
    PrettyTypeErrorBase (e -> Maybe Doc)                   -- ^
  | PrettyTypeErrorWithType ((ty nTy -> Doc) -> e -> Maybe Doc) -- ^

-- |
fixPrettyTypeErrorRule :: (ty nTy -> Doc)
                  -> PrettyTypeErrorRule e ty nTy
                  -> e
                  -> Maybe Doc
fixPrettyTypeErrorRule _ (PrettyTypeErrorBase f) x =
  f x
fixPrettyTypeErrorRule prettyType (PrettyTypeErrorWithType f) x =
  f prettyType x

-- |
data PrettyTypeErrorInput e ty nTy =
  PrettyTypeErrorInput [PrettyTypeErrorRule e ty nTy] -- ^

instance Monoid (PrettyTypeErrorInput e ty nTy) where
  mempty =
    PrettyTypeErrorInput mempty
  mappend (PrettyTypeErrorInput v1) (PrettyTypeErrorInput v2) =
    PrettyTypeErrorInput (mappend v1 v2)

-- |
data PrettyTypeErrorOutput e =
  PrettyTypeErrorOutput {
    _prettyTypeError      :: e -> Doc         -- ^
  , _prettyTypeErrorRules :: [e -> Maybe Doc] -- ^
  }

makeClassy ''PrettyTypeErrorOutput

-- |
mkPrettyTypeError :: PrettyTypeOutput ty
                  -> PrettyTypeErrorInput e ty nTy -- ^
                  -> PrettyTypeErrorOutput e -- ^
mkPrettyTypeError (PrettyTypeOutput prettyType _) (PrettyTypeErrorInput i) =
  let
    prettyTypeErrorRules' =
      fmap (fixPrettyTypeErrorRule prettyType) i
    prettyTypeError' ty =
      fromMaybe (text "???") .
      asum .
      fmap ($ ty) $
      prettyTypeErrorRules'
  in
    PrettyTypeErrorOutput
      prettyTypeError'
      prettyTypeErrorRules'
