{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Nat (
    natRules
  ) where

import           Component                           (ComponentInput (..))
import           Component.Term                      (TermInput (..))
import           Component.Type                      (TypeInput (..))
import           Component.Type.Error.Unexpected        (AsUnexpected)
import Component.Term.Parent (AsParentTerm)
import Component.Type.Parent (AsParentType)

import           Component.Term.Nat                (NatTerm, WithNatTerm)
import           Component.Term.Nat.Eval.BigStep   (bigStepInput)
import           Component.Term.Nat.Eval.SmallStep (smallStepInput)
import           Component.Term.Nat.Eval.Value     (valueInput)
import           Component.Term.Nat.Strip          (stripNoteTermInput)
import           Component.Term.Nat.Gen            (genTermInput)
import           Component.Term.Nat.Infer          (inferInput)
import           Component.Term.Nat.Parse          (parseTermInput)
import           Component.Term.Nat.Pretty         (prettyTermInput)
import           Component.Term.Nat.Size           (termSizeInput)
import           Component.Type.Nat                (NatType, WithNatType)
import           Component.Type.Nat.Strip          (stripNoteTypeInput)
import           Component.Type.Nat.Gen            (genTypeInput)
import           Component.Type.Nat.Parse          (parseTypeInput)
import           Component.Type.Nat.Pretty         (prettyTypeInput)

natRules :: ( Eq (ty nTy)
            , AsUnexpected e ty nTy
            , WithNatType ty nTy
            , WithNatTerm tm nTm a
            , AsParentType (NatType ty) ty
            , AsParentTerm (NatTerm tm) tm
            )
         => ComponentInput r e ty nTy tm nTm a
natRules =
    ComponentInput tyI mempty tmI
  where
    tyI =
      TypeInput
        stripNoteTypeInput
        genTypeInput
        parseTypeInput
        prettyTypeInput
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
