{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
module Language.NB.Type where

import Data.Monoid ((<>))
import Data.Bifunctor

import Control.Lens.TH (makeClassyPrisms, makeWrapped)
import Control.Lens.Wrapped (_Wrapped)

import Text.Trifecta.Rendering (Span)

import Common.Note
import Common.Type (TypeInput(..), TypeOutput(..), mkType)

import Components.Type.Nat.Data (NatType, AsNatType, _NatType)
import Components.Type.Bool.Data (BoolType, AsBoolType, _BoolType)
import Components.Type.Note.Data (NoteType, AsNoteType, _NoteType)

import qualified Components.Type.Nat as Nat
import qualified Components.Type.Bool as Bool
import qualified Components.Type.Note as Note

import Language.NB.Type.Error

data TypeF n f =
    TyNat NatType
  | TyBool BoolType
  | TyNote (NoteType n f)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TypeF

instance Bifunctor TypeF where
  bimap _ _ (TyNat n) = TyNat n
  bimap _ _ (TyBool b) = TyBool b
  bimap f g (TyNote n) = TyNote $ bimap f g n

instance WithoutNote f => WithoutNote (TypeF n f) where
  type Without (TypeF n f) = TypeF () (Without f)

  stripNote = bimap (const ()) stripNote

newtype Type n = Type { getType :: TypeF n (Type n) }
  deriving (Eq, Ord, Show)

makeWrapped ''Type

instance WithoutNote (Type n) where
  type Without (Type n) = Type ()
  stripNote (Type t) = Type (stripNote t)

instance AsNatType (Type n) where
  _NatType = _Wrapped . _TyNat

instance AsBoolType (Type n) where
  _BoolType = _Wrapped . _TyBool

instance AsNoteType (Type n) n (Type n) where
  _NoteType = _Wrapped . _TyNote

typeInput :: TypeInput (Error Span (Type Span)) (Type Span)
typeInput =
  Nat.typeInput <>
  Bool.typeInput <>
  Note.typeInput <>
  errorTypeInput

typeOutput :: TypeOutput (Error Span (Type Span)) (Type Span)
typeOutput =
  mkType typeInput
