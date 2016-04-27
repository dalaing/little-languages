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
module Component.Type.Pretty (
    PrettyTypeRule(..)
  , PrettyTypeInput(..)
  , PrettyTypeOutput(..)
  , HasPrettyTypeOutput(..)
  , mkPrettyType
  ) where

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Control.Lens.TH (makeClassy)
import Text.PrettyPrint.ANSI.Leijen (Doc, text, parens)

import Common.Text (ExpressionInfo(..))

-- |
data PrettyTypeRule ty =
    PrettyTypeBase (forall n. ty n -> Maybe Doc)                   -- ^
  | PrettyTypeExpression ExpressionInfo (forall n. ty n -> Maybe (ty n, ty n)) (Doc -> Doc -> Doc)
  | PrettyTypeRecurse (forall n. (ty n -> Doc) -> ty n -> Maybe Doc) -- ^

-- |
fixPrettyTypeRule :: (ty n -> Doc)
                  -> PrettyTypeRule ty
                  -> ty n
                  -> Maybe Doc
fixPrettyTypeRule _ (PrettyTypeBase f) x =
  f x
fixPrettyTypeRule prettyType (PrettyTypeExpression _ split pretty) x = do
  (ty1, ty2) <- split x
  return $ parens (pretty (prettyType ty1) (prettyType ty2))
fixPrettyTypeRule step (PrettyTypeRecurse f) x =
  f step x

-- |
data PrettyTypeInput ty =
  PrettyTypeInput [PrettyTypeRule ty] -- ^

instance Monoid (PrettyTypeInput ty) where
  mempty =
    PrettyTypeInput mempty
  mappend (PrettyTypeInput v1) (PrettyTypeInput v2) =
    PrettyTypeInput (mappend v1 v2)

-- |
data PrettyTypeOutput ty =
  PrettyTypeOutput {
    _prettyType      :: forall n. ty n -> Doc         -- ^
  , _prettyTypeRules :: forall n. [ty n -> Maybe Doc] -- ^
  }

makeClassy ''PrettyTypeOutput

-- |
mkPrettyType :: PrettyTypeInput ty  -- ^
             -> PrettyTypeOutput ty -- ^
mkPrettyType (PrettyTypeInput i) =
  let
    prettyTypeRules' =
      fmap (fixPrettyTypeRule prettyType') i
    prettyType' ty =
      fromMaybe (text "???") .
      asum .
      fmap ($ ty) $
      prettyTypeRules'
  in
    PrettyTypeOutput
      prettyType'
      prettyTypeRules'
