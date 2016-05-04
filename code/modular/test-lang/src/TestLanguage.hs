{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE RankNTypes           #-}
module TestLanguage (
    Type(..)
  , TypeError(..)
  , Term(..)
  , errorRules
  , errorRulesSrcLoc
  , languageRules
  ) where

import Control.Monad (ap)

import           Control.Lens.TH               (makeClassyPrisms)
import Control.Lens (review, view)
import           Data.Monoid                   ((<>))
import           Text.Trifecta.Rendering       (Renderable (..))
import Prelude.Extras (Eq1(..), Ord1(..), Show1(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Constraint (Dict(..))

import Bound2 (Bound3(..))
import Bifunctor2 (Bifunctor2(..))
import Common.Note (TranslateNote)
import           Component.Type.Error.ExpectedEq  (AsExpectedEq (..),
                                                ExpectedEq (..),
                                                expectedEqInput,
                                                expectedEqSrcLocInput)
import           Component.Type.Error.Unexpected  (AsUnexpected (..),
                                                Unexpected (..),
                                                unexpectedInput,
                                                unexpectedSrcLocInput)
import           Component.Type.Error.UnknownType (AsUnknownType (..),
                                                unknownTypeInput)
import           Component                     (ComponentInput, ComponentOutput)

import           Component.Bool               (boolRules)
import           Component.STLC               (stlcRules, stlcErrors, stlcSrcLocErrors)
import           Component.Term.Bool          (AsBoolTerm (..), BoolTerm (..))
import           Component.Term.STLC          (AsSTLCTerm (..), AsSTLCVar (..),
                                               STLCTerm (..), STLCVar (..))
import           Component.Type.STLC          (AsSTLCType(..), STLCType(..), Context(..))
import           Component.Type.Bool          (AsBoolType (..), BoolType (..))
import           Component.Nat                (natRules)
import           Component.NatBool                (natBoolRules)
import           Component.Term.Nat           (AsNatTerm (..), NatTerm (..))
import           Component.Term.NatBool           (AsNatBoolTerm (..), NatBoolTerm (..))
import           Component.Type.Nat           (AsNatType (..), NatType (..))
import           Component.Note                (noteRules)
import           Component.Term.Note           (AsNoteTerm (..), NoteTerm (..))
import           Component.Type.Note           (AsNoteType (..), NoteType (..))
import Component.Type.Error.FreeVar (FreeVar(..), AsFreeVar(..))
import Component.Type.Error.NotArrow (NotArrow(..), AsNotArrow(..))
import Component.Type.Note.Strip (StripNoteType(..))
import Component.Term.Note.Strip (StripNoteTerm(..))
import           Language                      (mkLanguageDefaultParser)

data Type n =
    BoolTy (BoolType Type n)
  | NatTy (NatType Type n)
  | StlcTy (STLCType Type n)
  | NoteTy (NoteType Type n)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Type

instance AsBoolType Type Type where
  _BoolType = _BoolTy

instance AsNatType Type Type where
  _NatType = _NatTy

instance AsSTLCType Type Type where
  _STLCType = _StlcTy

instance AsNoteType Type Type where
  _NoteType = _NoteTy

instance StripNoteType Type Type where
  mapMaybeNoteType f (BoolTy i) = mapMaybeNoteType f i
  mapMaybeNoteType f (NatTy n) = mapMaybeNoteType f n
  mapMaybeNoteType f (StlcTy n) = mapMaybeNoteType f n
  mapMaybeNoteType f (NoteTy n) = mapMaybeNoteType f n

data TypeError n a =
    TeUnknownType -- (Maybe n)
  | TeUnexpected (Unexpected Type n)
  | TeExpectedEq (ExpectedEq Type n)
  | TeFreeVar (FreeVar a)
  | TeNotArrow (NotArrow Type n)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TypeError

instance AsUnknownType (TypeError n a) where
  _UnknownType = _TeUnknownType

instance AsUnexpected (TypeError n a) Type n where
  _Unexpected = _TeUnexpected . _Unexpected

instance AsExpectedEq (TypeError n a) Type n where
  _ExpectedEq = _TeExpectedEq . _ExpectedEq

instance AsFreeVar (TypeError n a) a where
  _FreeVar = _TeFreeVar . _FreeVar

instance AsNotArrow (TypeError n a) Type n where
  _NotArrow = _TeNotArrow . _NotArrow

data Term nTy nTm a =
    TmBool (BoolTerm Term nTy nTm a)
  | TmNat (NatTerm Term nTy nTm a)
  | TmNatBool (NatBoolTerm Term nTy nTm a)
  | VarSTLC (STLCVar Term nTy nTm a)
  | TmSTLC (STLCTerm Type Term nTy nTm a)
  | TmNoted (NoteTerm Term nTy nTm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq nTy, Eq nTm) => Eq1 (Term nTy nTm) where
  (==#) = (==)

instance (Ord nTy, Ord nTm) => Ord1 (Term nTy nTm) where
  compare1 = compare

instance (Show nTy, Show nTm) => Show1 (Term nTy nTm) where
  showsPrec1 = showsPrec

instance Applicative (Term nTy nTm) where
  pure = return
  (<*>) = ap

instance Monad (Term nTy nTm) where
  return = review _TmVar

  VarSTLC (TmVar x) >>= f = f x
  TmBool tm >>= f = TmBool (tm >>>>>= f)
  TmNat tm >>= f = TmNat (tm >>>>>= f)
  TmNatBool tm >>= f = TmNatBool (tm >>>>>= f)
  TmSTLC tm >>= f = TmSTLC (tm >>>>>= f)
  TmNoted tm >>= f = TmNoted (tm >>>>>= f)

instance Bifunctor2 Term where
  bifunctor2 _ = Dict

makeClassyPrisms ''Term

instance AsBoolTerm Term Term where
  _BoolTerm = _TmBool

instance AsNatTerm Term Term where
  _NatTerm = _TmNat

instance AsNatBoolTerm Term Term where
  _NatBoolTerm = _TmNatBool

instance AsSTLCVar Term Term where
  _STLCVar = _VarSTLC

instance AsSTLCTerm Term Type Term where
  _STLCTerm = _TmSTLC

instance AsNoteTerm Term Term where
  _NoteTerm = _TmNoted

instance StripNoteTerm Term Term where
  mapMaybeNoteTerm f (TmBool i) = mapMaybeNoteTerm f i
  mapMaybeNoteTerm f (TmNat i) = mapMaybeNoteTerm f i
  mapMaybeNoteTerm f (TmNatBool i) = mapMaybeNoteTerm f i
  mapMaybeNoteTerm f (VarSTLC i) = mapMaybeNoteTerm f i
  mapMaybeNoteTerm f (TmSTLC i) = mapMaybeNoteTerm f i
  mapMaybeNoteTerm f (TmNoted n) = mapMaybeNoteTerm f n

instance Bifunctor (Term nTy) where
  bimap l r (TmBool i) = TmBool (bimap l r i)
  bimap l r (TmNat i) = TmNat (bimap l r i)
  bimap l r (TmNatBool i) = TmNatBool (bimap l r i)
  bimap l r (VarSTLC i) = VarSTLC (bimap l r i)
  bimap l r (TmSTLC i) = TmSTLC (bimap l r i)
  bimap l r (TmNoted i) = TmNoted (bimap l r i)

instance Bifoldable (Term nTy) where
  bifoldMap l r (TmBool i) = bifoldMap l r i
  bifoldMap l r (TmNat i) = bifoldMap l r i
  bifoldMap l r (TmNatBool i) = bifoldMap l r i
  bifoldMap l r (VarSTLC i) = bifoldMap l r i
  bifoldMap l r (TmSTLC i) = bifoldMap l r i
  bifoldMap l r (TmNoted i) = bifoldMap l r i

instance Bitraversable (Term nTy) where
  bitraverse l r (TmBool i) = TmBool <$> bitraverse l r i
  bitraverse l r (TmNat i) = TmNat <$> bitraverse l r i
  bitraverse l r (TmNatBool i) = TmNatBool <$> bitraverse l r i
  bitraverse l r (VarSTLC i) = VarSTLC <$> bitraverse l r i
  bitraverse l r (TmSTLC i) = TmSTLC <$> bitraverse l r i
  bitraverse l r (TmNoted i) = TmNoted <$> bitraverse l r i

errorRules :: ComponentInput r (TypeError n String) Type n Term n String
errorRules =
  unknownTypeInput <>
  unexpectedInput <>
  expectedEqInput <>
  stlcErrors

errorRulesSrcLoc :: ( Show n
                    , Renderable n
                    )
                 => ComponentInput r (TypeError n String) Type n Term n String
errorRulesSrcLoc =
  unknownTypeInput <>
  unexpectedSrcLocInput <>
  expectedEqSrcLocInput <>
  stlcSrcLocErrors

languageRules :: ( Eq n
                 , Show n
                 , TranslateNote n n
                 )
              => ComponentInput (Context Type n String) (TypeError n String) Type n Term n String
languageRules =
  boolRules <>
  natRules <>
  natBoolRules <>
  stlcRules <>
  noteRules
