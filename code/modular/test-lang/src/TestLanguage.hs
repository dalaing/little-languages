{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module TestLanguage (
    Type(..)
  , TypeError(..)
  , Term(..)
  , errorRules
  , errorRulesSrcLoc
  , languageRules
  ) where

import           Data.Monoid                   ((<>))
import           Control.Lens.TH               (makeClassyPrisms)
import Text.Trifecta.Rendering (Renderable(..))

import           Common.Type.Error.ExpectedEq  (AsExpectedEq (..),
                                                ExpectedEq (..),
                                                expectedEqInput, expectedEqSrcLocInput)
import           Common.Type.Error.Unexpected  (AsUnexpected (..),
                                                Unexpected (..),
                                                unexpectedInput, unexpectedSrcLocInput)
import           Common.Type.Error.UnknownType (AsUnknownType (..),
                                                unknownTypeInput)
import           Component                     (ComponentInput, ComponentOutput)

import           Components.Bool               (boolRules)
import           Components.Term.Bool          (AsBoolTerm (..), BoolTerm (..))
import           Components.Type.Bool          (AsBoolType (..), BoolType (..))
import           Components.Nat                (natRules)
import           Components.Term.Nat           (AsNatTerm (..), NatTerm (..))
import           Components.Type.Nat           (AsNatType (..), NatType (..))
import           Component.Note                (noteRules)
import           Component.Term.Note           (AsNoteTerm (..), NoteTerm (..))
import           Component.Type.Note           (AsNoteType (..), NoteType (..))
import           Language                      (mkLanguageDefaultParser)

data Type n =
    BoolTy BoolType
  | NatTy NatType
  | NoteTy (NoteType n (Type n))
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Type

instance AsBoolType (Type n) where
  _BoolType = _BoolTy

instance AsNatType (Type n) where
  _NatType = _NatTy

instance AsNoteType (Type n) n (Type n) where
  _NoteType = _NoteTy

data TypeError n =
    TeUnknownType -- (Maybe n)
  | TeUnexpected (Unexpected (Type n))
  | TeExpectedEq (ExpectedEq (Type n))
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TypeError

instance AsUnknownType (TypeError n) where
  _UnknownType = _TeUnknownType

instance AsUnexpected (TypeError n) (Type n) where
  _Unexpected = _TeUnexpected . _Unexpected

instance AsExpectedEq (TypeError n) (Type n) where
  _ExpectedEq = _TeExpectedEq . _ExpectedEq

data Term n a =
    TmBool (BoolTerm (Term n) a)
  | TmNat (NatTerm (Term n) a)
  | TmNoted (NoteTerm n (Term n) a)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Term

instance AsBoolTerm (Term n a) (Term n) a where
  _BoolTerm = _TmBool

instance AsNatTerm (Term n a) (Term n) a where
  _NatTerm = _TmNat

instance AsNoteTerm (Term n a) n (Term n) a where
  _NoteTerm = _TmNoted

errorRules :: ComponentInput (TypeError n) (Type n) (Term n) a
errorRules =
  unknownTypeInput <>
  unexpectedInput <>
  expectedEqInput

errorRulesSrcLoc :: ( Show n
                    , Renderable n
                    )
                 => ComponentInput (TypeError n) (Type n) (Term n) a
errorRulesSrcLoc =
  unknownTypeInput <>
  unexpectedSrcLocInput <>
  expectedEqSrcLocInput

languageRules :: ( Eq n
                 , Show n
                 )
              => ComponentInput (TypeError n) (Type n) (Term n) a
languageRules =
  boolRules <>
  natRules <>
  noteRules
