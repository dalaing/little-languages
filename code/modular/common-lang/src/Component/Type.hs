{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Component.Type (
    TypeInput(..)
  , TypeOutput(..)
  , HasTypeOutput(..)
  , mkType
  ) where


import           Control.Lens.TH       (makeClassy)

import           Common.Parse          (GetReservedWords (..),
                                        ParserHelperOutput)
-- import           Component.Type.Note.Strip (StripNoteTypeInput(..), StripNoteTypeOutput, mkStripNoteType)
import           Component.Type.Gen    (GenTypeInput (..), GenTypeOutput (..),
                                        mkGenType)
import           Component.Type.Note   (WithNoteType)
import           Component.Type.Parse  (ParseTypeInput (..),
                                        ParseTypeOutput (..), mkParseType)
import           Component.Type.Pretty (PrettyTypeInput (..),
                                        PrettyTypeOutput (..), mkPrettyType)

data TypeInput ty nTy =
  TypeInput {
    _genTypeInput    :: GenTypeInput ty nTy
  , _parseTypeInput  :: ParseTypeInput ty
  , _prettyTypeInput :: PrettyTypeInput ty
  }

instance GetReservedWords (TypeInput ty nTy) where
  reservedWords = reservedWords . _parseTypeInput

instance Monoid (TypeInput ty nTy) where
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

data TypeOutput ty nTy =
  TypeOutput {
    _toGenTypeOutput    :: GenTypeOutput ty nTy
  , _toParseTypeOutput  :: ParseTypeOutput ty
  , _toPrettyTypeOutput :: PrettyTypeOutput ty
  }

makeClassy ''TypeOutput

mkType :: WithNoteType ty
       => ParserHelperOutput
       -> TypeInput ty nTy
       -> TypeOutput ty nTy
mkType h (TypeInput g pa pr) =
  TypeOutput
    (mkGenType g)
    (mkParseType h pa)
    (mkPrettyType pr)
