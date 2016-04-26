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
import Text.PrettyPrint.ANSI.Leijen (Doc, text, parens)

import Common.Text (ExpressionInfo(..))
import Component.Type.Pretty (PrettyTypeOutput(..))

-- |
data PrettyTermRule ty nTy tm nTm a =
    PrettyTermBase (tm nTm a -> Maybe Doc)                        -- ^
  | PrettyTermRecurse ((tm nTm a -> Doc) -> tm nTm a -> Maybe Doc) -- ^
  | PrettyTermExpression ExpressionInfo (tm nTm a -> Maybe (tm nTm a, tm nTm a)) (Doc -> Doc -> Doc)
  | PrettyTermWithType ((ty nTy -> Doc) -> (tm nTm a -> Doc) -> tm nTm a -> Maybe Doc) -- ^

-- |
fixPrettyTermRule :: (ty nTy -> Doc)
                  -> (tm nTm a -> Doc)
                  -> PrettyTermRule ty nTy tm nTm a
                  -> tm nTm a
                  -> Maybe Doc
fixPrettyTermRule _ _ (PrettyTermBase f) x =
  f x
fixPrettyTermRule _ prettyTerm (PrettyTermRecurse f) x =
  f prettyTerm x
fixPrettyTermRule _ prettyTerm (PrettyTermExpression _ split pretty) x = do
  (tm1, tm2) <- split x
  return $ parens (pretty (prettyTerm tm1) (prettyTerm tm2))
fixPrettyTermRule prettyType prettyTerm (PrettyTermWithType f) x =
  f prettyType prettyTerm x

-- |
data PrettyTermInput ty nTy tm nTm a =
  PrettyTermInput [PrettyTermRule ty nTy tm nTm a] -- ^

instance Monoid (PrettyTermInput ty nTy tm nTm a) where
  mempty =
    PrettyTermInput mempty
  mappend (PrettyTermInput v1) (PrettyTermInput v2) =
    PrettyTermInput (mappend v1 v2)

-- |
data PrettyTermOutput tm nTm a =
  PrettyTermOutput {
    _prettyTerm      :: tm nTm a -> Doc         -- ^
  , _prettyTermRules :: [tm nTm a -> Maybe Doc] -- ^
  }

makeClassy ''PrettyTermOutput

-- |
mkPrettyTerm :: PrettyTypeOutput ty nTy
             -> PrettyTermInput ty nTy tm nTm a  -- ^
             -> PrettyTermOutput tm nTm a -- ^
mkPrettyTerm (PrettyTypeOutput prettyType _) (PrettyTermInput i) =
  let
    prettyTermRules' =
      fmap (fixPrettyTermRule prettyType prettyTerm') i
    prettyTerm' tm =
      fromMaybe (text "???") .
      asum .
      fmap ($ tm) $
      prettyTermRules'
  in
    PrettyTermOutput
      prettyTerm'
      prettyTermRules'
