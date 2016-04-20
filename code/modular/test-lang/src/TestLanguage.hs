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

import Bound2 (Bound2(..))
import Component.Term.Parent (AsParentTerm(..))
import Component.Type.Parent (AsParentType(..))
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
-- import           Component.STLC               (stlcRules)
import           Component.Term.Bool          (AsBoolTerm (..), BoolTerm (..))
-- import           Component.Term.STLC          (AsSTLCTerm (..), AsSTLCVar (..),
--                                                STLCTerm (..), STLCVar (..))
import           Component.Type.Bool          (AsBoolType (..), BoolType (..))
import           Component.Nat                (natRules)
import           Component.Term.Nat           (AsNatTerm (..), NatTerm (..))
import           Component.Type.Nat           (AsNatType (..), NatType (..))
import           Component.Note                (noteRules)
import           Component.Term.Note           (AsNoteTerm (..), NoteTerm (..))
import           Component.Type.Note           (AsNoteType (..), NoteType (..))
import           Language                      (mkLanguageDefaultParser)

data Type n =
    BoolTy (BoolType Type n)
  | NatTy (NatType Type n)
  | NoteTy (NoteType Type n)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Type

instance AsBoolType (Type n) Type n where
  _BoolType = _BoolTy

instance AsParentType (BoolType Type) Type where
  _ParentType = _BoolTy

instance AsNatType (Type n) Type n where
  _NatType = _NatTy

instance AsParentType (NatType Type) Type where
  _ParentType = _NatTy

instance AsNoteType (Type n) Type n where
  _NoteType = _NoteTy

instance AsParentType (NoteType Type) Type where
  _ParentType = _NoteTy

data TypeError n =
    TeUnknownType -- (Maybe n)
  | TeUnexpected (Unexpected Type n)
  | TeExpectedEq (ExpectedEq Type n)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TypeError

instance AsUnknownType (TypeError n) where
  _UnknownType = _TeUnknownType

instance AsUnexpected (TypeError n) Type n where
  _Unexpected = _TeUnexpected . _Unexpected

instance AsExpectedEq (TypeError n) Type n where
  _ExpectedEq = _TeExpectedEq . _ExpectedEq

data Term n a =
    TmBool (BoolTerm Term n a)
  | TmNat (NatTerm Term n a)
--  | VarSTLC (STLCVar Term n a)
--  | TmSTLC (STLCTerm (Type n) Term n a)
  | TmNoted (NoteTerm Term n a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq n => Eq1 (Term n) where
  (==#) = (==)

instance Ord n => Ord1 (Term n) where
  compare1 = compare

instance Show n => Show1 (Term n) where
  showsPrec1 = showsPrec

{-
instance Applicative (Term n) where
  pure = return
  (<*>) = ap

instance Monad (Term n) where
  return = review _TmVar

  VarSTLC (TmVar x) >>= f = f x
  TmBool tm >>= f = TmBool (tm >>>>= f)
  TmNat tm >>= f = TmNat (tm >>>>= f)
  TmSTLC tm >>= f = TmSTLC (tm >>>>= f)
  TmNoted tm >>= f = TmNoted (tm >>>>= f)
-}

makeClassyPrisms ''Term

instance AsBoolTerm (Term n a) Term n a where
  _BoolTerm = _TmBool

instance AsParentTerm (BoolTerm Term) Term where
  _ParentTerm = _TmBool

{-
instance AsSTLCVar (Term n a) Term n a where
  _STLCVar = _VarSTLC

instance AsSTLCTerm (Term n a) (Type n) Term n a where
  _STLCTerm = _TmSTLC
-}

instance AsNatTerm (Term n a) Term n a where
  _NatTerm = _TmNat

instance AsParentTerm (NatTerm Term) Term where
  _ParentTerm = _TmNat

instance AsNoteTerm (Term n a) Term n a where
  _NoteTerm = _TmNoted

instance AsParentTerm (NoteTerm Term) Term where
  _ParentTerm = _TmNoted

errorRules :: ComponentInput () (TypeError n) Type n Term n a
errorRules =
  unknownTypeInput <>
  unexpectedInput <>
  expectedEqInput

errorRulesSrcLoc :: ( Show n
                    , Renderable n
                    )
                 => ComponentInput () (TypeError n) Type n Term n a
errorRulesSrcLoc =
  unknownTypeInput <>
  unexpectedSrcLocInput <>
  expectedEqSrcLocInput

languageRules :: ( Eq n
                 , Show n
                 , TranslateNote n n
                 )
              => ComponentInput () (TypeError n) Type n Term n a
languageRules =
  boolRules <>
  natRules <>
--  stlcRules <>
  noteRules
