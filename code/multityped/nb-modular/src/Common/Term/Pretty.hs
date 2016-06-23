{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Term.Pretty (
    PrettyTermInput(..)
  , HasPrettyTermInput(..)
  , PrettyTermOutput(..)
  , HasPrettyTermOutput(..)
  , mkPrettyTerm
  ) where

import Control.Lens.TH (makeClassy)

import Common.Recursion

import Text.PrettyPrint.ANSI.Leijen

data PrettyTermInput tm =
  PrettyTermInput {
    _prettyTermSteps :: [MaybeStep tm Doc]
  }

makeClassy ''PrettyTermInput

instance Monoid (PrettyTermInput tm) where
  mempty = PrettyTermInput mempty
  mappend (PrettyTermInput p1) (PrettyTermInput p2) =
    PrettyTermInput (mappend p1 p2)

data PrettyTermOutput tm =
  PrettyTermOutput {
    _prettyTerm :: tm -> Doc
  , _prettyTermString :: tm -> String
  }

makeClassy ''PrettyTermOutput

mkPrettyTerm :: PrettyTermInput tm
             -> PrettyTermOutput tm
mkPrettyTerm (PrettyTermInput ps) =
  let
    prTm = combineMaybeSteps (text "???") ps
  in
    PrettyTermOutput
      prTm
      (docString . prTm)

docString :: Doc
          -> String
docString d =
  displayS (renderPretty 0.3 80 (plain d)) ""
