{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Language.NB.Term where

import Data.Monoid ((<>))
import Data.Bifunctor

import Control.Lens.TH (makeClassyPrisms, makeWrapped)
import Control.Lens.Wrapped (_Wrapped)

import Text.Trifecta.Rendering (Span)

import Common.Note
import Common.Type
import Common.Term (TermInput(..), TermOutput(..), mkTerm)

import Components.Term.Nat.Data
import Components.Term.Bool.Data
import Components.Term.NatBool.Data
import Components.Term.Note.Data

import qualified Components.Term.Nat as Nat
import qualified Components.Term.Bool as Bool
import qualified Components.Term.NatBool as NatBool
import qualified Components.Term.Note as Note

import Language.NB.Type
import Language.NB.Type.Error

data TermF t n f =
    TmNat (NatTerm t f)
  | TmBool (BoolTerm t f)
  | TmNatBool (NatBoolTerm t f)
  | TmNote (NoteTerm t n f)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TermF

instance Bifunctor (TermF t) where
  bimap _ g (TmNat n)     = TmNat (fmap g n)
  bimap _ g (TmBool b)    = TmBool (fmap g b)
  bimap _ g (TmNatBool n) = TmNatBool (fmap g n)
  bimap f g (TmNote n)    = TmNote (bimap f g n)

instance WithoutNote f => WithoutNote (TermF t n f) where
  type Without (TermF t n f) = TermF t () (Without f)

  stripNote = bimap (const ()) stripNote

newtype Term t n = Term { getTerm :: TermF t n (Term t n) }
  deriving (Eq, Ord, Show)

makeWrapped ''Term

instance WithoutNote (Term t n) where
  type Without (Term t n) = Term t ()

  stripNote (Term t) = Term (stripNote t)

instance AsNatTerm (Term t n) t (Term t n) where
  _NatTerm = _Wrapped . _TmNat

instance AsBoolTerm (Term t n) t (Term t n) where
  _BoolTerm = _Wrapped . _TmBool

instance AsNatBoolTerm (Term t n) t (Term t n) where
  _NatBoolTerm = _Wrapped . _TmNatBool

instance AsNoteTerm (Term t n) t n (Term t n) where
  _NoteTerm = _Wrapped . _TmNote

cataTerm :: (TermF t n a -> a) -> Term t n -> a
cataTerm f = f . fmap (cataTerm f) . getTerm

termInput :: TermInput (Error Span (Type Span)) (Type Span) (Term (Type Span) Span)
termInput =
  Nat.termInput <>
  Bool.termInput <>
  NatBool.termInput <>
  Note.termInput

termOutput :: TypeOutput (Error Span (Type Span)) (Type Span)
           -> TermOutput (Error Span (Type Span)) (Type Span) (Term (Type Span) Span)
termOutput ty =
  mkTerm ty termInput
