module Components.Term.Note.Eval.Value (
    valueInput
  ) where

import Common.Term.Eval.Value (ValueInput(..))

import Components.Term.Note.Data

valueInput :: WithNoteTerm n ty tm
           => ValueInput tm
valueInput =
  mempty
