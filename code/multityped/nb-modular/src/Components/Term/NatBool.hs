{-# LANGUAGE FlexibleContexts #-}
module Components.Term.NatBool where

import Common.Note
import Common.Term
import Common.Term.Eval
import Common.Type.Error

import Components.Term.Nat.Data (WithNatTerm)
import Components.Term.Bool.Data (WithBoolTerm)
import Components.Term.NatBool.Data
import Components.Term.NatBool.Gen
import Components.Term.NatBool.Infer
import Components.Term.NatBool.Eval.Value
import Components.Term.NatBool.Eval.SmallStep
import Components.Term.NatBool.Eval.BigStep
import Components.Term.NatBool.Parse
import Components.Term.NatBool.Pretty

termInput :: ( Eq (Without ty)
             , WithoutNote ty
             , AsUnexpected e ty
             , WithNatBoolTerm ty tm
             , WithNatTerm ty tm
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
