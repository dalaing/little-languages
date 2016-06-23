{-# LANGUAGE FlexibleContexts #-}
module Components.Term.Note where

import Text.Trifecta.Rendering (Span)

import Common.Term
import Common.Term.Eval

import Components.Type.Note.Data

import Components.Term.Note.Data
import Components.Term.Note.Gen
import Components.Term.Note.Infer
import Components.Term.Note.Eval.Value
import Components.Term.Note.Eval.SmallStep
import Components.Term.Note.Eval.BigStep
import Components.Term.Note.Parse
import Components.Term.Note.Pretty

termInput :: ( WithNoteType Span ty
             , WithNoteTerm Span ty tm
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
