{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Type (
    TypeInput(..)
  , HasTypeInput(..)
  , TypeOutput(..)
  , HasTypeOutput(..)
  , mkType
  ) where

import Control.Lens.TH (makeClassy)

import Common.Type.Gen
import Common.Type.Parse
import Common.Type.Pretty
import Common.Type.Error.Pretty

data TypeInput e ty =
  TypeInput {
    _tiGenTypeInput :: GenTypeInput ty
  , _tiParseTypeInput :: ParseTypeInput ty
  , _tiPrettyTypeInput :: PrettyTypeInput ty
  , _tiPrettyTypeErrorInput :: PrettyTypeErrorInput e ty
  }

makeClassy ''TypeInput

instance HasGenTypeInput (TypeInput e ty) ty where
  genTypeInput = tiGenTypeInput

instance HasParseTypeInput (TypeInput e ty) ty where
  parseTypeInput = tiParseTypeInput

instance HasPrettyTypeInput (TypeInput e ty) ty where
  prettyTypeInput = tiPrettyTypeInput

instance HasPrettyTypeErrorInput (TypeInput e ty) e ty where
  prettyTypeErrorInput = tiPrettyTypeErrorInput

instance Monoid (TypeInput e ty) where
  mempty =
    TypeInput mempty mempty mempty mempty
  mappend (TypeInput g1 pa1 pr1 e1) (TypeInput g2 pa2 pr2 e2) =
    TypeInput
      (mappend g1 g2)
      (mappend pa1 pa2)
      (mappend pr1 pr2)
      (mappend e1 e2)

data TypeOutput e ty =
  TypeOutput {
    _toGenTypeOutput :: GenTypeOutput ty
  , _toParseTypeOutput :: ParseTypeOutput ty
  , _toPrettyTypeOutput :: PrettyTypeOutput ty
  , _toPrettyTypeErrorOutput :: PrettyTypeErrorOutput e ty
  }

makeClassy ''TypeOutput

instance HasGenTypeOutput (TypeOutput e ty) ty where
  genTypeOutput = toGenTypeOutput

instance HasParseTypeOutput (TypeOutput e ty) ty where
  parseTypeOutput = toParseTypeOutput

instance HasPrettyTypeOutput (TypeOutput e ty) ty where
  prettyTypeOutput = toPrettyTypeOutput

instance HasPrettyTypeErrorOutput (TypeOutput e ty) e ty where
  prettyTypeErrorOutput = toPrettyTypeErrorOutput

mkType :: TypeInput e ty
       -> TypeOutput e ty
mkType (TypeInput g pa pr e) =
  let
    prTy = mkPrettyType pr
  in
  TypeOutput
    (mkGenType g)
    (mkParseType pa)
    prTy
    (mkPrettyTypeError prTy e)
