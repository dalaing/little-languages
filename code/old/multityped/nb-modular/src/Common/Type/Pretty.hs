{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Type.Pretty (
    PrettyTypeInput(..)
  , HasPrettyTypeInput(..)
  , PrettyTypeOutput(..)
  , HasPrettyTypeOutput(..)
  , mkPrettyType
  ) where

import Control.Lens.TH (makeClassy)

import Text.PrettyPrint.ANSI.Leijen

import Common.Recursion

data PrettyTypeInput ty =
  PrettyTypeInput {
    _prettyTypeSteps :: [MaybeStep ty Doc]
  }

makeClassy ''PrettyTypeInput

instance Monoid (PrettyTypeInput ty) where
  mempty = PrettyTypeInput mempty
  mappend (PrettyTypeInput p1) (PrettyTypeInput p2) =
    PrettyTypeInput (mappend p1 p2)

data PrettyTypeOutput ty =
  PrettyTypeOutput {
    _prettyType :: ty -> Doc
  , _prettyTypeString :: ty -> String
  }

makeClassy ''PrettyTypeOutput

mkPrettyType :: PrettyTypeInput ty
             -> PrettyTypeOutput ty
mkPrettyType (PrettyTypeInput ps) =
  let
    prTy = combineMaybeSteps (text "???") ps
  in
    PrettyTypeOutput
      prTy
      (docString . prTy)

docString :: Doc
          -> String
docString d =
  displayS (renderPretty 0.3 80 (plain d)) ""
