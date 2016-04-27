{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.NatBool.Eval.Value (
    valueInput
  ) where

import Component.Term.Eval.Value (ValueInput(..))

import Component.Term.NatBool (WithNatBoolTerm)

valueInput :: WithNatBoolTerm tm
           => ValueInput tm n a
valueInput = mempty
