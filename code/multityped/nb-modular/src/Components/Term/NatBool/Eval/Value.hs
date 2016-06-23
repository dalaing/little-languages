module Components.Term.NatBool.Eval.Value (
    valueInput
  ) where

import Common.Term.Eval.Value (ValueInput(..))

import Components.Term.NatBool.Data

valueInput :: WithNatBoolTerm ty tm
           => ValueInput tm
valueInput =
  mempty
