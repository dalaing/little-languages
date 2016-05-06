{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.NatBool (
    natBoolRules
  ) where

import           Component                           (ComponentInput (..))
import           Component.Term                      (TermInput (..))
import           Component.Type.Error.Unexpected        (AsUnexpected)
import Extras (Eq1)

import           Component.Term.NatBool                (NatBoolTerm, WithNatBoolTerm)
import           Component.Term.NatBool.Eval.BigStep   (bigStepInput)
import           Component.Term.NatBool.Eval.SmallStep (smallStepInput)
import           Component.Term.NatBool.Eval.Value     (valueInput)
import           Component.Term.NatBool.Gen            (genTermInput)
import           Component.Term.NatBool.Infer          (inferInput)
import           Component.Term.NatBool.Parse          (parseTermInput)
import           Component.Term.NatBool.Pretty         (prettyTermInput)
import           Component.Term.NatBool.SubTerm        (subTermInput)
import           Component.Type.Nat (WithNatType)
import           Component.Type.Bool (WithBoolType)

natBoolRules :: ( Eq1 ty
                , AsUnexpected e ty
                , WithNatBoolTerm tm
                , WithNatType ty
                , WithBoolType ty
                )
             => ComponentInput r e ty tm
natBoolRules =
    ComponentInput mempty mempty tmI
  where
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
