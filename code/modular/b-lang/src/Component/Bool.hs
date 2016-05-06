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
import Extras (Eq1)

import           Component.Term.Bool                (WithBoolTerm)
import           Component.Term.Bool.Eval.BigStep   (bigStepInput)
import           Component.Term.Bool.Eval.SmallStep (smallStepInput)
import           Component.Term.Bool.Eval.Value     (valueInput)
import           Component.Term.Bool.Gen            (genTermInput)
import           Component.Term.Bool.Infer          (inferInput)
import           Component.Term.Bool.Parse          (parseTermInput)
import           Component.Term.Bool.Pretty         (prettyTermInput)
import           Component.Term.Bool.SubTerm        (subTermInput)
import           Component.Type.Bool                (WithBoolType)
import           Component.Type.Bool.Gen            (genTypeInput)
import           Component.Type.Bool.Parse          (parseTypeInput)
import           Component.Type.Bool.Pretty         (prettyTypeInput)

boolRules :: ( Eq1 ty 
             , AsUnexpected e ty
             , AsExpectedEq e ty
             , WithBoolType ty
             , WithBoolTerm tm
             )
          => ComponentInput r e ty tm
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
        subTermInput
        genTermInput
        parseTermInput
        prettyTermInput
        inferInput 
        valueInput
        smallStepInput
        bigStepInput

