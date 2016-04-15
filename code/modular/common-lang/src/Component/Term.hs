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


import           Control.Lens.TH                     (makeClassy)

import           Common.Parse                        (GetReservedWords (..),
                                                      ParserHelperOutput)
import           Common.Type.Error.UnknownType.Class (AsUnknownType (..))
import           Component.Term.Eval.BigStep         (BigStepInput (..),
                                                      BigStepOutput, mkBigStep)
import           Component.Term.Eval.SmallStep       (SmallStepInput (..),
                                                      SmallStepOutput,
                                                      mkSmallStep)
import           Component.Term.Eval.Value           (ValueInput (..),
                                                      ValueOutput, mkValue)
import           Component.Term.Gen                  (GenTermInput (..),
                                                      GenTermOutput, mkGenTerm)
import           Component.Term.Infer                (InferInput (..),
                                                      InferOutput, mkInfer)
import           Component.Term.Parse                (ParseTermInput (..),
                                                      ParseTermOutput,
                                                      mkParseTerm)
import           Component.Term.Pretty               (PrettyTermInput (..),
                                                      PrettyTermOutput,
                                                      mkPrettyTerm)
import           Component.Term.Size                 (TermSizeInput (..),
                                                      TermSizeOutput,
                                                      mkTermSize)

data TermInput e ty tm a =
  TermInput {
    _termSizeInput   :: TermSizeInput (tm a)
  , _genTermInput    :: GenTermInput (tm a)
  , _parseTermInput  :: ParseTermInput tm a
  , _prettyTermInput :: PrettyTermInput (tm a)
  , _inferInput      :: InferInput e ty (tm a)
  , _valueInput      :: ValueInput (tm a)
  , _smallStepInput  :: SmallStepInput (tm a)
  , _bigStepInput    :: BigStepInput (tm a)
  }

instance GetReservedWords (TermInput e ty tm a) where
  reservedWords = reservedWords . _parseTermInput

instance Monoid (TermInput e ty tm a) where
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

data TermOutput e ty tm a =
  TermOutput {
    _toTermSizeOutput   :: TermSizeOutput (tm a)
  , _toGenTermOutput    :: GenTermOutput (tm a)
  , _toParseTermOutput  :: ParseTermOutput tm a
  , _toPrettyTermOutput :: PrettyTermOutput (tm a)
  , _toInferOutput      :: InferOutput e ty (tm a)
  , _toValueOutput      :: ValueOutput (tm a)
  , _toSmallStepOutput  :: SmallStepOutput (tm a)
  , _toBigStepOutput    :: BigStepOutput (tm a)
  }

makeClassy ''TermOutput

mkTerm :: AsUnknownType e
       => ParserHelperOutput
       -> TermInput e ty tm a
       -> TermOutput e ty tm a
mkTerm h (TermInput t g pa pr i v s b) =
  let
    vo = mkValue v
  in
    TermOutput
      (mkTermSize t)
      (mkGenTerm g)
      (mkParseTerm h pa)
      (mkPrettyTerm pr)
      (mkInfer i)
      vo
      (mkSmallStep vo s)
      (mkBigStep b)
