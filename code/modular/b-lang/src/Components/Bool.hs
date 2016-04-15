{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Bool (
    boolRules
  ) where

import           Common.Type.Error.ExpectedEq        (AsExpectedEq)
import           Common.Type.Error.Unexpected        (AsUnexpected)
import           Component                           (ComponentInput (..))
import           Component.Term                      (TermInput (..))
import           Component.Type                      (TypeInput (..))

import           Components.Term.Bool                (WithBoolTerm)
import           Components.Term.Bool.Eval.BigStep   (bigStepInput)
import           Components.Term.Bool.Eval.SmallStep (smallStepInput)
import           Components.Term.Bool.Eval.Value     (valueInput)
import           Components.Term.Bool.Gen            (genTermInput)
import           Components.Term.Bool.Infer          (inferInput)
import           Components.Term.Bool.Parse          (parseTermInput)
import           Components.Term.Bool.Pretty         (prettyTermInput)
import           Components.Term.Bool.Size           (termSizeInput)
import           Components.Type.Bool                (AsBoolType (..))
import           Components.Type.Bool.Gen            (genTypeInput)
import           Components.Type.Bool.Parse          (parseTypeInput)
import           Components.Type.Bool.Pretty         (prettyTypeInput)

boolRules :: ( Eq ty
             , AsUnexpected e ty
             , AsExpectedEq e ty
             , AsBoolType ty
             , WithBoolTerm tm a
             )
          => ComponentInput e ty tm a
boolRules =
    ComponentInput tyI mempty tmI
  where
    tyI =
      TypeInput
        genTypeInput
        parseTypeInput
        prettyTypeInput
    tmI =
      TermInput
        termSizeInput
        genTermInput
        parseTermInput
        prettyTermInput
        inferInput
        valueInput
        smallStepInput
        bigStepInput

