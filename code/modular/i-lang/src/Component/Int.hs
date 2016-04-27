{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Int (
    intRules
  ) where

import           Component                           (ComponentInput (..))
import           Component.Term                      (TermInput (..))
import           Component.Type                      (TypeInput (..))
import           Component.Type.Error.Unexpected        (AsUnexpected)

import           Component.Term.Int                (IntTerm, WithIntTerm)
import           Component.Term.Int.Eval.BigStep   (bigStepInput)
import           Component.Term.Int.Eval.SmallStep (smallStepInput)
import           Component.Term.Int.Eval.Value     (valueInput)
import           Component.Term.Int.Gen            (genTermInput)
import           Component.Term.Int.Infer          (inferInput)
import           Component.Term.Int.Parse          (parseTermInput)
import           Component.Term.Int.Pretty         (prettyTermInput)
import           Component.Term.Int.Size           (termSizeInput)
import           Component.Type.Int                (IntType, WithIntType)
import           Component.Type.Int.Gen            (genTypeInput)
import           Component.Type.Int.Parse          (parseTypeInput)
import           Component.Type.Int.Pretty         (prettyTypeInput)

intRules :: ( Eq (ty nTy)
            , AsUnexpected e ty nTy
            , WithIntType ty
            , WithIntTerm tm
            )
         => ComponentInput r e ty nTy tm nTm a
intRules =
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
