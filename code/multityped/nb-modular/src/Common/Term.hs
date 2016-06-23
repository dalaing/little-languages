{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Term where

import Control.Lens.TH (makeClassy)

import Common.Type
import Common.Type.Error

import Common.Term.Size
import Common.Term.Gen
import Common.Term.Infer
import Common.Term.Eval
import Common.Term.Eval.Value
import Common.Term.Eval.SmallStep
import Common.Term.Eval.BigStep
import Common.Term.Parse
import Common.Term.Pretty

data TermInput e ty tm =
  TermInput {
    _tiSizeInput :: SizeInput tm
  , _tiGenTermInput :: GenTermInput ty tm
  , _tiInferInput :: InferInput e ty tm
  , _tiEvalInput :: EvalInput tm
  , _tiParseTermInput :: ParseTermInput tm
  , _tiPrettyTermInput :: PrettyTermInput tm
  }

makeClassy ''TermInput

instance HasSizeInput (TermInput e ty tm) tm where
 sizeInput = tiSizeInput

instance HasGenTermInput (TermInput e ty tm) ty tm where
  genTermInput = tiGenTermInput

instance HasInferInput (TermInput e ty tm) e ty tm where
  inferInput = tiInferInput

instance HasValueInput (TermInput e ty tm) tm where
  valueInput = tiEvalInput . valueInput

instance HasSmallStepInput (TermInput e ty tm) tm where
  smallStepInput = tiEvalInput . smallStepInput

instance HasBigStepInput (TermInput e ty tm) tm where
  bigStepInput = tiEvalInput . bigStepInput

instance Monoid (TermInput e ty tm) where
  mempty =
    TermInput mempty mempty mempty mempty mempty mempty
  mappend (TermInput s1 g1 i1 e1 pa1 pr1) (TermInput s2 g2 i2 e2 pa2 pr2) =
    TermInput
      (mappend s1 s2)
      (mappend g1 g2)
      (mappend i1 i2)
      (mappend e1 e2)
      (mappend pa1 pa2)
      (mappend pr1 pr2)

data TermOutput e ty tm =
  TermOutput {
    _toSizeOutput :: SizeOutput tm
  , _toGenTermOutput :: GenTermOutput ty tm
  , _toInferOutput :: InferOutput e ty tm
  , _toEvalOutput :: EvalOutput tm
  , _toParseTermOutput :: ParseTermOutput tm
  , _toPrettyTermOutput :: PrettyTermOutput tm
  }

makeClassy ''TermOutput

instance HasSizeOutput (TermOutput e ty tm) tm where
  sizeOutput = toSizeOutput

instance HasGenTermOutput (TermOutput e ty tm) ty tm where
  genTermOutput = toGenTermOutput

instance HasInferOutput (TermOutput e ty tm) e ty tm where
  inferOutput = toInferOutput

instance HasValueOutput (TermOutput e ty tm) tm where
  valueOutput = toEvalOutput . valueOutput

instance HasSmallStepOutput (TermOutput e ty tm) tm where
  smallStepOutput = toEvalOutput . smallStepOutput

instance HasBigStepOutput (TermOutput e ty tm) tm where
  bigStepOutput = toEvalOutput . bigStepOutput

instance HasParseTermOutput (TermOutput e ty tm) tm where
  parseTermOutput = toParseTermOutput

instance HasPrettyTermOutput (TermOutput e ty tm) tm where
  prettyTermOutput = toPrettyTermOutput

mkTerm :: AsUnknownType e n
       => TypeOutput e ty
       -> TermInput e ty tm
       -> TermOutput e ty tm
mkTerm (TypeOutput gTy _ _ _) (TermInput s gTm i e pa pr) =
  TermOutput
    (mkSize s)
    (mkGenTerm gTy gTm)
    (mkInfer i)
    (mkEval e)
    (mkParseTerm pa)
    (mkPrettyTerm pr)
