{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Bool (
    boolRules
  ) where

import           Component                           (ComponentInput (..))
import           Component.Term                      (TermInput (..))
import           Component.Type                      (TypeInput (..))
import           Component.Type.Error.ExpectedEq        (AsExpectedEq)
import           Component.Type.Error.Unexpected        (AsUnexpected)

import           Component.Term.Bool                (BoolTerm, WithBoolTerm)
import           Component.Term.Bool.Eval.BigStep   (bigStepInput)
import           Component.Term.Bool.Eval.SmallStep (smallStepInput)
import           Component.Term.Bool.Eval.Value     (valueInput)
import           Component.Term.Bool.Gen            (genTermInput)
import           Component.Term.Bool.Infer          (inferInput)
import           Component.Term.Bool.Parse          (parseTermInput)
import           Component.Term.Bool.Pretty         (prettyTermInput)
import           Component.Term.Bool.Size           (termSizeInput)
import           Component.Type.Bool                (BoolType, WithBoolType)
import           Component.Type.Bool.Gen            (genTypeInput)
import           Component.Type.Bool.Parse          (parseTypeInput)
import           Component.Type.Bool.Pretty         (prettyTypeInput)

boolRules :: ( Eq (ty nTy)
             , AsUnexpected e ty nTy
             , AsExpectedEq e ty nTy
             , WithBoolType ty
             , WithBoolTerm tm
             )
          => ComponentInput r e ty nTy tm nTm a
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

