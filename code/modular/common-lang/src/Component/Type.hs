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
module Component.Type (
    TypeInput(..)
  , TypeOutput(..)
  , HasTypeOutput(..)
  , mkType
  ) where


import           Control.Lens.TH           (makeClassy)

import           Common.Parse              (ParserHelperOutput, GetReservedWords(..))
import           Component.Type.Gen (GenTypeInput(..), GenTypeOutput(..), mkGenType)
import           Component.Type.Parse (ParseTypeInput(..), ParseTypeOutput(..), mkParseType)
import           Component.Type.Pretty (PrettyTypeInput(..), PrettyTypeOutput(..), mkPrettyType)

data TypeInput ty =
  TypeInput {
    _genTypeInput         :: GenTypeInput ty
  , _parseTypeInput       :: ParseTypeInput ty
  , _prettyTypeInput      :: PrettyTypeInput ty
  }

instance GetReservedWords (TypeInput ty) where
  reservedWords = reservedWords . _parseTypeInput

instance Monoid (TypeInput ty) where
  mempty =
    TypeInput
      mempty
      mempty
      mempty
  mappend (TypeInput g1 pa1 pr1) (TypeInput g2 pa2 pr2) =
    TypeInput
      (mappend g1 g2)
      (mappend pa1 pa2)
      (mappend pr1 pr2)

data TypeOutput ty =
  TypeOutput {
    _toGenTypeOutput         :: GenTypeOutput ty
  , _toParseTypeOutput       :: ParseTypeOutput ty
  , _toPrettyTypeOutput      :: PrettyTypeOutput ty
  }

makeClassy ''TypeOutput

mkType :: ParserHelperOutput
       -> TypeInput ty
       -> TypeOutput ty
mkType h (TypeInput g pa pr) =
  TypeOutput
    (mkGenType g)
    (mkParseType h pa)
    (mkPrettyType pr)
