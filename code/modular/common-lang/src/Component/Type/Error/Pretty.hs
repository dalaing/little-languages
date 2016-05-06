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
import Text.Trifecta.Rendering (Renderable)

import Component.Type.Pretty (PrettyTypeOutput(..))

-- |
data PrettyTypeErrorRule e ty =
    PrettyTypeErrorBase (forall nTy. e nTy String -> Maybe Doc)                   -- ^
  | PrettyTypeErrorBaseSrcLoc (forall nTy. Renderable nTy => e nTy String -> Maybe Doc)                   -- ^
  | PrettyTypeErrorWithType (forall nTy. (ty nTy -> Doc) -> e nTy String -> Maybe Doc) -- ^
  | PrettyTypeErrorWithTypeSrcLoc (forall nTy. Renderable nTy => (ty nTy -> Doc) -> e nTy String -> Maybe Doc) -- ^

-- |
fixPrettyTypeErrorRule :: (ty nTy -> Doc)
                  -> PrettyTypeErrorRule e ty
                  -> e nTy String
                  -> Maybe Doc
fixPrettyTypeErrorRule _ (PrettyTypeErrorBase f) x =
  f x
fixPrettyTypeErrorRule prettyType (PrettyTypeErrorWithType f) x =
  f prettyType x
fixPrettyTypeErrorRule _ _ _ =
  Nothing

fixPrettyTypeErrorRuleSrcLoc :: Renderable nTy
                             => (ty nTy -> Doc)
                             -> PrettyTypeErrorRule e ty
                             -> e nTy String
                             -> Maybe Doc
fixPrettyTypeErrorRuleSrcLoc _ (PrettyTypeErrorBaseSrcLoc f) x =
  f x
fixPrettyTypeErrorRuleSrcLoc prettyType (PrettyTypeErrorWithTypeSrcLoc f) x =
  f prettyType x
fixPrettyTypeErrorRuleSrcLoc _ _ _ =
  Nothing

-- |
data PrettyTypeErrorInput e ty =
  PrettyTypeErrorInput [PrettyTypeErrorRule e ty] -- ^

instance Monoid (PrettyTypeErrorInput e ty) where
  mempty =
    PrettyTypeErrorInput mempty
  mappend (PrettyTypeErrorInput v1) (PrettyTypeErrorInput v2) =
    PrettyTypeErrorInput (mappend v1 v2)

-- |
data PrettyTypeErrorOutput e =
  PrettyTypeErrorOutput {
    _prettyTypeError       :: forall nTy. e nTy String -> Doc         -- ^
  , _prettyTypeErrorSrcLoc :: forall nTy. Renderable nTy => e nTy String -> Doc         -- ^
  }

makeClassy ''PrettyTypeErrorOutput

-- |
mkPrettyTypeError :: PrettyTypeOutput ty
                  -> PrettyTypeErrorInput e ty -- ^
                  -> PrettyTypeErrorOutput e -- ^
mkPrettyTypeError (PrettyTypeOutput prettyType _) (PrettyTypeErrorInput i) =
  let
    prettyTypeError' ty =
      fromMaybe (text "???") .
      asum .
      fmap ($ ty) .
      fmap (fixPrettyTypeErrorRule prettyType) $
      i
    prettyTypeErrorSrcLoc' ty =
      fromMaybe (text "???") .
      asum .
      fmap ($ ty) .
      fmap (fixPrettyTypeErrorRuleSrcLoc prettyType) $
      i
  in
    PrettyTypeErrorOutput
      prettyTypeError'
      prettyTypeErrorSrcLoc'
