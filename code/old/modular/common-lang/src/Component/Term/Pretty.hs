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
data PrettyTermRule ty tm =
    PrettyTermBase (forall nTy nTm. tm nTy nTm String -> Maybe Doc)                        -- ^
  | PrettyTermRecurse (forall nTy nTm. (tm nTy nTm String -> Doc) -> tm nTy nTm String -> Maybe Doc) -- ^
  | PrettyTermExpression ExpressionInfo (forall nTy nTm. tm nTy nTm String -> Maybe (tm nTy nTm String, tm nTy nTm String)) (Doc -> Doc -> Doc)
  | PrettyTermWithType (forall nTy nTm. (ty nTy -> Doc) -> (tm nTy nTm String -> Doc) -> tm nTy nTm String -> Maybe Doc) -- ^

-- |
fixPrettyTermRule :: (ty nTy -> Doc)
                  -> (tm nTy nTm String -> Doc)
                  -> PrettyTermRule ty tm
                  -> tm nTy nTm String
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
data PrettyTermInput ty tm =
  PrettyTermInput [PrettyTermRule ty tm] -- ^

instance Monoid (PrettyTermInput ty tm) where
  mempty =
    PrettyTermInput mempty
  mappend (PrettyTermInput v1) (PrettyTermInput v2) =
    PrettyTermInput (mappend v1 v2)

-- |
data PrettyTermOutput tm =
  PrettyTermOutput {
    _prettyTerm      :: forall nTy nTm. tm nTy nTm String -> Doc         -- ^
  , _prettyTermRules :: forall nTy nTm. [tm nTy nTm String -> Maybe Doc] -- ^
  }

makeClassy ''PrettyTermOutput

-- |
mkPrettyTerm :: PrettyTypeOutput ty
             -> PrettyTermInput ty tm -- ^
             -> PrettyTermOutput tm -- ^
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
