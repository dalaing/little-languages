{-# LANGUAGE FlexibleContexts #-}
module Components.Term.Bool where

import Common.Note
import Common.Term
import Common.Term.Eval
import Common.Type.Error

import Components.Term.Bool.Data
import Components.Term.Bool.Gen
import Components.Term.Bool.Infer
import Components.Term.Bool.Eval.Value
import Components.Term.Bool.Eval.SmallStep
import Components.Term.Bool.Eval.BigStep
import Components.Term.Bool.Parse
import Components.Term.Bool.Pretty

termInput :: ( Eq (Without ty)
             , WithoutNote ty
             , AsUnexpected e ty
             , AsExpectedEq e ty
             , WithBoolTerm ty tm
             )
          => TermInput e ty tm
termInput =
  TermInput
    sizeInput
    genTermInput
    inferInput
    (EvalInput
      valueInput
      smallStepInput
      bigStepInput
    )
    parseTermInput
    prettyTermInput
