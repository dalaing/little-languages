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
import           Component.Type.Note.Strip (StripNoteTypeInput(..), StripNoteTypeOutput, mkStripNoteType)
import           Component.Type.Gen (GenTypeInput(..), GenTypeOutput(..), mkGenType)
import           Component.Type.Parse (ParseTypeInput(..), ParseTypeOutput(..), mkParseType)
import           Component.Type.Pretty (PrettyTypeInput(..), PrettyTypeOutput(..), mkPrettyType)

data TypeInput ty n =
  TypeInput {
    _stripNoteTypeInput :: StripNoteTypeInput ty
  , _genTypeInput       :: GenTypeInput ty n
  , _parseTypeInput     :: ParseTypeInput ty n
  , _prettyTypeInput    :: PrettyTypeInput ty n
  }

instance GetReservedWords (TypeInput ty n) where
  reservedWords = reservedWords . _parseTypeInput

instance Monoid (TypeInput ty n) where
  mempty =
    TypeInput
      mempty
      mempty
      mempty
      mempty
  mappend (TypeInput s1 g1 pa1 pr1) (TypeInput s2 g2 pa2 pr2) =
    TypeInput
      (mappend s1 s2)
      (mappend g1 g2)
      (mappend pa1 pa2)
      (mappend pr1 pr2)

data TypeOutput ty n =
  TypeOutput {
    _toStripNoteTypeOutput :: StripNoteTypeOutput ty
  , _toGenTypeOutput       :: GenTypeOutput ty n
  , _toParseTypeOutput     :: ParseTypeOutput ty n
  , _toPrettyTypeOutput    :: PrettyTypeOutput ty n
  }

makeClassy ''TypeOutput

mkType :: ParserHelperOutput
       -> TypeInput ty n
       -> TypeOutput ty n
mkType h (TypeInput s g pa pr) =
  TypeOutput
    (mkStripNoteType s)
    (mkGenType g)
    (mkParseType h pa)
    (mkPrettyType pr)
