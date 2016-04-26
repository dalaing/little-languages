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
import Component.Term.Parent (AsParentTerm)

import           Component.Term.NatBool                (NatBoolTerm, WithNatBoolTerm)
import           Component.Term.NatBool.Eval.BigStep   (bigStepInput)
import           Component.Term.NatBool.Eval.SmallStep (smallStepInput)
import           Component.Term.NatBool.Eval.Value     (valueInput)
import           Component.Term.NatBool.Strip          (stripNoteTermInput)
import           Component.Term.NatBool.Gen            (genTermInput)
import           Component.Term.NatBool.Infer          (inferInput)
import           Component.Term.NatBool.Parse          (parseTermInput)
import           Component.Term.NatBool.Pretty         (prettyTermInput)
import           Component.Term.NatBool.Size           (termSizeInput)
import           Component.Type.Nat (WithNatType)
import           Component.Type.Bool (WithBoolType)

natBoolRules :: ( Eq (ty nTy)
                , AsUnexpected e ty nTy
                , WithNatBoolTerm tm nTm a
                , WithNatType ty nTy
                , WithBoolType ty nTy
                , AsParentTerm (NatBoolTerm tm) tm
                )
             => ComponentInput r e ty nTy tm nTm a
natBoolRules =
    ComponentInput mempty mempty tmI
  where
    tmI =
      TermInput
        termSizeInput
        stripNoteTermInput
        genTermInput
        parseTermInput
        prettyTermInput
        inferInput
        valueInput
        smallStepInput
        bigStepInput
