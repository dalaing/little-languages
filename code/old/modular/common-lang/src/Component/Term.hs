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
import           Component.Term.Note                    (WithNoteTerm)
import           Component.Term.Note.Strip              (StripNoteTerm)
import           Component.Term.Parse                   (ParseTermInput (..),
                                                         ParseTermOutput,
                                                         mkParseTerm)
import           Component.Term.Pretty                  (PrettyTermInput (..),
                                                         PrettyTermOutput,
                                                         mkPrettyTerm)
import           Component.Term.SubTerm                 (SubTermInput (..),
                                                         SubTermOutput,
                                                         mkSubTerm)
import           Component.Type                         (TypeOutput (..))
import           Component.Type.Error.UnknownType.Class (AsUnknownType (..))
import           Component.Type.Note.Strip              (StripNoteType)
import Extras (Eq1, Eq3, Show3)

data TermInput r e ty tm =
  TermInput {
    _subTermInput    :: SubTermInput tm
  , _genTermInput    :: GenTermInput ty tm
  , _parseTermInput  :: ParseTermInput ty tm
  , _prettyTermInput :: PrettyTermInput ty tm
  , _inferInput      :: InferInput r e ty tm
  , _valueInput      :: ValueInput tm
  , _smallStepInput  :: SmallStepInput tm
  , _bigStepInput    :: BigStepInput tm
  }

instance GetReservedWords (TermInput r e ty tm) where
  reservedWords = reservedWords . _parseTermInput

instance Monoid (TermInput r e ty tm) where
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
  mappend (TermInput t1 g1 pa1 pr1 i1 v1 s1 b1) (TermInput t2 g2 pa2 pr2 i2 v2 s2 b2) =
    TermInput
      (mappend t1 t2)
      (mappend g1 g2)
      (mappend pa1 pa2)
      (mappend pr1 pr2)
      (mappend i1 i2)
      (mappend v1 v2)
      (mappend s1 s2)
      (mappend b1 b2)

data TermOutput r e ty tm =
  TermOutput {
    _toSubTermOutput    :: SubTermOutput tm
  , _toGenTermOutput    :: GenTermOutput ty tm
  , _toParseTermOutput  :: ParseTermOutput tm
  , _toPrettyTermOutput :: PrettyTermOutput tm
  , _toInferOutput      :: InferOutput r e ty tm
  , _toValueOutput      :: ValueOutput tm
  , _toSmallStepOutput  :: SmallStepOutput tm
  , _toBigStepOutput    :: BigStepOutput tm
  }

makeClassy ''TermOutput

mkTerm :: ( AsUnknownType e
          , Eq3 tm
          , Eq1 ty
          , Show3 tm
          , WithNoteTerm tm
          , StripNoteTerm tm tm
          , StripNoteType ty ty
          )
       => ParserHelperOutput
       -> TypeOutput ty
       -> TermInput r e ty tm
       -> TermOutput r e ty tm
mkTerm h (TypeOutput gty paty prty) (TermInput t g pa pr i v s b) =
  let
    vo = mkValue v
  in
    TermOutput
      (mkSubTerm t)
      (mkGenTerm gty g)
      (mkParseTerm h paty pa)
      (mkPrettyTerm prty pr)
      (mkInfer i)
      vo
      (mkSmallStep vo s)
      (mkBigStep b)
