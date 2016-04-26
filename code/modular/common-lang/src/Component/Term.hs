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
module Component.Term (
    TermInput(..)
  , TermOutput(..)
  , HasTermOutput(..)
  , mkTerm
  ) where


import           Control.Lens.TH                        (makeClassy)

import           Common.Parse                           (GetReservedWords (..),
                                                         ParserHelperOutput)
import           Component.Term.Eval.BigStep            (BigStepInput (..),
                                                         BigStepOutput,
                                                         mkBigStep)
import           Component.Term.Eval.SmallStep          (SmallStepInput (..),
                                                         SmallStepOutput,
                                                         mkSmallStep)
import           Component.Term.Eval.Value              (ValueInput (..),
                                                         ValueOutput, mkValue)
import           Component.Term.Gen                     (GenTermInput (..),
                                                         GenTermOutput,
                                                         mkGenTerm)
import           Component.Term.Infer                   (InferInput (..),
                                                         InferOutput, mkInfer)
import           Component.Term.Note.Strip              (StripNoteTermInput (..),
                                                         StripNoteTermOutput,
                                                         mkStripNoteTerm)
import           Component.Term.Parse                   (ParseTermInput (..),
                                                         ParseTermOutput,
                                                         mkParseTerm)
import           Component.Term.Pretty                  (PrettyTermInput (..),
                                                         PrettyTermOutput,
                                                         mkPrettyTerm)
import           Component.Term.Size                    (TermSizeInput (..),
                                                         TermSizeOutput,
                                                         mkTermSize)
import           Component.Type                         (TypeOutput (..))
import           Component.Type.Error.UnknownType.Class (AsUnknownType (..))

data TermInput r e ty nTy tm nTm a =
  TermInput {
    _termSizeInput      :: TermSizeInput tm nTm a
  , _stripNoteTermInput :: StripNoteTermInput tm
  , _genTermInput       :: GenTermInput tm nTm a
  , _parseTermInput     :: ParseTermInput tm nTm a
  , _prettyTermInput    :: PrettyTermInput tm nTm a
  , _inferInput         :: InferInput r e ty nTy tm nTm a
  , _valueInput         :: ValueInput tm nTm a
  , _smallStepInput     :: SmallStepInput tm nTm a
  , _bigStepInput       :: BigStepInput tm nTm a
  }

instance GetReservedWords (TermInput r e ty nTy tm nTm a) where
  reservedWords = reservedWords . _parseTermInput

instance Monoid (TermInput r e ty nTy tm nTm a) where
  mempty =
    TermInput
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
  mappend (TermInput t1 st1 g1 pa1 pr1 i1 v1 s1 b1) (TermInput t2 st2 g2 pa2 pr2 i2 v2 s2 b2) =
    TermInput
      (mappend t1 t2)
      (mappend st1 st2)
      (mappend g1 g2)
      (mappend pa1 pa2)
      (mappend pr1 pr2)
      (mappend i1 i2)
      (mappend v1 v2)
      (mappend s1 s2)
      (mappend b1 b2)

data TermOutput r e ty nTy tm nTm a =
  TermOutput {
    _toTermSizeOutput      :: TermSizeOutput tm nTm a
  , _toStripNoteTermOutput :: StripNoteTermOutput tm
  , _toGenTermOutput       :: GenTermOutput tm nTm a
  , _toParseTermOutput     :: ParseTermOutput tm nTm a
  , _toPrettyTermOutput    :: PrettyTermOutput tm nTm a
  , _toInferOutput         :: InferOutput r e ty nTy tm nTm a
  , _toValueOutput         :: ValueOutput tm nTm a
  , _toSmallStepOutput     :: SmallStepOutput tm nTm a
  , _toBigStepOutput       :: BigStepOutput tm nTm a
  }

makeClassy ''TermOutput

mkTerm :: AsUnknownType e
       => ParserHelperOutput
       -> TypeOutput ty nTy
       -> TermInput r e ty nTy tm nTm a
       -> TermOutput r e ty nTy tm nTm a
mkTerm h (TypeOutput snty _ _ _) (TermInput t st g pa pr i v s b) =
  let
    sntm = mkStripNoteTerm st
    vo = mkValue sntm v
  in
    TermOutput
      (mkTermSize t)
      sntm
      (mkGenTerm g)
      (mkParseTerm h pa)
      (mkPrettyTerm pr)
      (mkInfer snty i)
      vo
      (mkSmallStep sntm vo s)
      (mkBigStep sntm b)