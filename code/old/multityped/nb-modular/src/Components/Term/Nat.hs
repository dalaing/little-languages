{-# LANGUAGE FlexibleContexts #-}
module Components.Term.Nat where

import Common.Note
import Common.Term
import Common.Term.Eval
import Common.Type.Error

import Components.Term.Nat.Data
import Components.Term.Nat.Gen
import Components.Term.Nat.Infer
import Components.Term.Nat.Eval.Value
import Components.Term.Nat.Eval.SmallStep
import Components.Term.Nat.Eval.BigStep
import Components.Term.Nat.Parse
import Components.Term.Nat.Pretty

termInput :: ( Eq (Without ty)
             , WithoutNote ty
             , AsUnexpected e ty
             , WithNatTerm ty tm
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
