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
import Extras (Eq1)

import           Component.Term.Nat                (WithNatTerm)
import           Component.Term.Nat.Eval.BigStep   (bigStepInput)
import           Component.Term.Nat.Eval.SmallStep (smallStepInput)
import           Component.Term.Nat.Eval.Value     (valueInput)
import           Component.Term.Nat.Gen            (genTermInput)
import           Component.Term.Nat.Infer          (inferInput)
import           Component.Term.Nat.Parse          (parseTermInput)
import           Component.Term.Nat.Pretty         (prettyTermInput)
import           Component.Term.Nat.SubTerm        (subTermInput)
import           Component.Type.Nat                (WithNatType)
import           Component.Type.Nat.Gen            (genTypeInput)
import           Component.Type.Nat.Parse          (parseTypeInput)
import           Component.Type.Nat.Pretty         (prettyTypeInput)

natRules :: ( Eq1 ty
            , AsUnexpected e ty
            , WithNatType ty
            , WithNatTerm tm
            )
         => ComponentInput r e ty tm
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
        subTermInput
        genTermInput
        parseTermInput
        prettyTermInput
        inferInput
        valueInput
        smallStepInput
        bigStepInput
