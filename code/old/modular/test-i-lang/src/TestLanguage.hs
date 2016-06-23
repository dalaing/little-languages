{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ScopedTypeVariables          #-}
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
import Data.Constraint

import Bound2 (Bound2(..))
import qualified Extras (Eq1(..), Show1(..), Eq3(..), Show3(..))
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

import           Component.Int                (intRules)
import           Component.Term.Int           (AsIntTerm (..), IntTerm (..))
import           Component.Type.Int           (AsIntType (..), IntType (..))
import           Component.Note                (noteRules)
import           Component.Term.Note           (AsNoteTerm (..), NoteTerm (..))
import           Component.Type.Note           (AsNoteType (..), NoteType (..))
import Component.Type.Note.Strip (StripNoteType(..))
import Component.Term.Note.Strip (StripNoteTerm(..))
import           Language                      (mkLanguageDefaultParser)

data Type n =
    IntTy (IntType Type n)
  | NoteTy (NoteType Type n)
  deriving (Eq, Ord, Show)

instance Extras.Eq1 Type where
  spanEq1 = Sub Dict

instance Extras.Show1 Type where
  spanShow1 = Sub Dict

makeClassyPrisms ''Type

instance AsIntType Type Type where
  _IntType = _IntTy

instance AsNoteType Type Type where
  _NoteType = _NoteTy

instance StripNoteType Type Type where
  mapMaybeNoteType f (IntTy i) = mapMaybeNoteType f i
  mapMaybeNoteType f (NoteTy n) = mapMaybeNoteType f n

data TypeError n =
    TeUnknownType -- (Maybe n)
  | TeUnexpected (Unexpected Type n)
  | TeExpectedEq (ExpectedEq Type n)
  deriving (Eq, Ord, Show)

instance Extras.Eq1 TypeError where
  spanEq1 = Sub Dict

instance Extras.Show1 TypeError where
  spanShow1 = Sub Dict

makeClassyPrisms ''TypeError

instance AsUnknownType TypeError where
  _UnknownType = _TeUnknownType

instance AsUnexpected TypeError Type where
  _Unexpected = _TeUnexpected . _Unexpected

instance AsExpectedEq TypeError Type where
  _ExpectedEq = _TeExpectedEq . _ExpectedEq

data Term nTy nTm a =
    TmInt (IntTerm Term nTy nTm a)
  | TmNoted (NoteTerm Term nTy nTm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Extras.Eq3 Term where
  spanEq3 = Sub Dict

instance Extras.Show3 Term where
  spanShow3 = Sub Dict

makeClassyPrisms ''Term

instance AsIntTerm Term Term where
  _IntTerm = _TmInt

instance AsNoteTerm Term Term where
  _NoteTerm = _TmNoted

instance StripNoteTerm Term Term where
  mapMaybeNoteTerm f (TmInt i) = mapMaybeNoteTerm f i
  mapMaybeNoteTerm f (TmNoted n) = mapMaybeNoteTerm f n

errorRules :: ComponentInput r TypeError Type Term
errorRules =
  unknownTypeInput <>
  unexpectedInput <>
  expectedEqInput

errorRulesSrcLoc :: ComponentInput r TypeError Type Term
errorRulesSrcLoc =
  unknownTypeInput <>
  unexpectedSrcLocInput <>
  expectedEqSrcLocInput

languageRules :: ComponentInput () TypeError Type Term
languageRules =
  (intRules <> noteRules)
   -- \\ (Sub Dict :: (Eq nTy) :- Eq (Term nTy nTm String))
