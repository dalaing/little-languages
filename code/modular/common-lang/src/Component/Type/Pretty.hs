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
data PrettyTypeRule ty n =
    PrettyTypeBase (ty n -> Maybe Doc)                   -- ^
  | PrettyTypeExpression ExpressionInfo (ty n -> Maybe (ty n, ty n)) (Doc -> Doc -> Doc)
  | PrettyTypeRecurse ((ty n -> Doc) -> ty n -> Maybe Doc) -- ^

-- |
fixPrettyTypeRule :: (ty n -> Doc)
                  -> PrettyTypeRule ty n
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
data PrettyTypeInput ty n =
  PrettyTypeInput [PrettyTypeRule ty n] -- ^

instance Monoid (PrettyTypeInput ty n) where
  mempty =
    PrettyTypeInput mempty
  mappend (PrettyTypeInput v1) (PrettyTypeInput v2) =
    PrettyTypeInput (mappend v1 v2)

-- |
data PrettyTypeOutput ty n =
  PrettyTypeOutput {
    _prettyType      :: ty n -> Doc         -- ^
  , _prettyTypeRules :: [ty n -> Maybe Doc] -- ^
  }

makeClassy ''PrettyTypeOutput

-- |
mkPrettyType :: PrettyTypeInput ty n  -- ^
             -> PrettyTypeOutput ty n -- ^
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
