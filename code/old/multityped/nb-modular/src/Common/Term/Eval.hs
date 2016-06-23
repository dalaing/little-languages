{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Term.Eval (
    EvalInput(..)
  , HasEvalInput(..)
  , EvalOutput(..)
  , HasEvalOutput(..)
  , mkEval
  ) where

import Control.Lens.TH (makeClassy)

import Common.Term.Eval.Value
import Common.Term.Eval.SmallStep
import Common.Term.Eval.BigStep

data EvalInput tm =
  EvalInput {
    _eiValueInput :: ValueInput tm
  , _eiSmallStepInput :: SmallStepInput tm
  , _eiBigStepInput :: BigStepInput tm
  }

makeClassy ''EvalInput

instance HasValueInput (EvalInput tm) tm where
  valueInput = eiValueInput

instance HasSmallStepInput (EvalInput tm) tm where
  smallStepInput = eiSmallStepInput

instance HasBigStepInput (EvalInput tm) tm where
  bigStepInput = eiBigStepInput

instance Monoid (EvalInput tm) where
  mempty =
    EvalInput mempty mempty mempty
  mappend (EvalInput v1 s1 b1) (EvalInput v2 s2 b2) =
    EvalInput (mappend v1 v2) (mappend s1 s2) (mappend b1 b2)

data EvalOutput tm =
  EvalOutput {
    _eoValueOutput :: ValueOutput tm
  , _eoSmallStepOutput :: SmallStepOutput tm
  , _eoBigStepOutput :: BigStepOutput tm
  }

makeClassy ''EvalOutput

instance HasValueOutput (EvalOutput tm) tm where
  valueOutput = eoValueOutput

instance HasSmallStepOutput (EvalOutput tm) tm where
  smallStepOutput = eoSmallStepOutput

instance HasBigStepOutput (EvalOutput tm) tm where
  bigStepOutput = eoBigStepOutput

mkEval :: EvalInput tm
       -> EvalOutput tm
mkEval (EvalInput v s b) =
  EvalOutput (mkValue v) (mkSmallStep s) (mkBigStep b)
