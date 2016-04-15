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
import Text.PrettyPrint.ANSI.Leijen (Doc, text)

-- |
data PrettyTypeRule ty =
    PrettyTypeBase (ty -> Maybe Doc)                   -- ^
  | PrettyTypeRecurse ((ty -> Doc) -> ty -> Maybe Doc) -- ^

-- |
fixPrettyTypeRule :: (ty -> Doc)
                  -> PrettyTypeRule ty
                  -> ty
                  -> Maybe Doc
fixPrettyTypeRule _ (PrettyTypeBase f) x =
  f x
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
    _prettyType      :: ty -> Doc         -- ^
  , _prettyTypeRules :: [ty -> Maybe Doc] -- ^
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
