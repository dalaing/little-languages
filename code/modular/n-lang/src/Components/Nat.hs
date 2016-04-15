{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Nat (
    natRules
  ) where

import           Common.Type.Error.Unexpected        (AsUnexpected)
import           Component                           (ComponentInput (..))
import           Component.Term                      (TermInput (..))
import           Component.Type                      (TypeInput (..))

import           Components.Term.Nat                (WithNatTerm)
import           Components.Term.Nat.Eval.BigStep   (bigStepInput)
import           Components.Term.Nat.Eval.SmallStep (smallStepInput)
import           Components.Term.Nat.Eval.Value     (valueInput)
import           Components.Term.Nat.Gen            (genTermInput)
import           Components.Term.Nat.Infer          (inferInput)
import           Components.Term.Nat.Parse          (parseTermInput)
import           Components.Term.Nat.Pretty         (prettyTermInput)
import           Components.Term.Nat.Size           (termSizeInput)
import           Components.Type.Nat                (AsNatType (..))
import           Components.Type.Nat.Gen            (genTypeInput)
import           Components.Type.Nat.Parse          (parseTypeInput)
import           Components.Type.Nat.Pretty         (prettyTypeInput)

natRules :: ( Eq ty
            , AsUnexpected e ty
            , AsNatType ty
            , WithNatTerm tm a
            )
         => ComponentInput e ty tm a
natRules =
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

