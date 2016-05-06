{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.STLC (
    stlcErrors
  , stlcSrcLocErrors
  , stlcRules
  ) where

import Text.Trifecta.Rendering (Renderable)
import Data.Constraint.Forall (ForallT)

import           Component                           (ComponentInput (..))
import           Component.Term                      (TermInput (..))
import           Component.Type                      (TypeInput (..))
import Component.Type.Note (WithNoteType)
import           Component.Type.Error.Unexpected        (AsUnexpected)
import Extras (Eq1)

import           Component.Term.STLC                (WithSTLCTerm)
import           Component.Term.STLC.SubTerm        (subTermInput)
import           Component.Term.STLC.Eval.Value     (valueInput)
import           Component.Term.STLC.Eval.SmallStep (smallStepInput)
import           Component.Term.STLC.Eval.BigStep   (bigStepInput)
-- import           Component.Term.STLC.Gen            (genTermInput)
import           Component.Term.STLC.Infer          (inferInput)
import           Component.Term.STLC.Parse          (parseTermInput)
import           Component.Term.STLC.Pretty         (prettyTermInput)
import           Component.Type.STLC                (WithSTLCType, HasContext)
-- import           Component.Type.STLC.Gen            (genTypeInput)
import           Component.Type.STLC.Parse          (parseTypeInput)
import           Component.Type.STLC.Pretty         (prettyTypeInput)
import           Component.Type.Error.FreeVar       (freeVarInput, AsFreeVar)
import           Component.Type.Error.NotArrow      (notArrowInput, notArrowSrcLocInput, AsNotArrow)

stlcErrors :: ( AsFreeVar e
              , AsNotArrow e ty
              )
           => ComponentInput r e ty tm
stlcErrors =
  ComponentInput
    mempty
    (mappend freeVarInput notArrowInput)
    mempty

stlcSrcLocErrors :: ( AsFreeVar e
                    , AsNotArrow e ty
                    , WithNoteType ty
                    -- , Renderable nTy
                    )
                 => ComponentInput r e ty tm
stlcSrcLocErrors =
  ComponentInput
    mempty
    (mappend freeVarInput notArrowSrcLocInput)
    mempty

stlcRules :: ( Eq1 ty
             , AsUnexpected e ty
             , AsNotArrow e ty
             , AsFreeVar e
             , HasContext r ty
             , WithSTLCType ty
             , WithSTLCTerm tm ty
             , ForallT Monad tm
             )
          => ComponentInput r e ty tm
stlcRules =
    ComponentInput tyI mempty tmI
  where
    tyI =
      TypeInput
        mempty -- genTypeInput
        parseTypeInput
        prettyTypeInput
    tmI =
      TermInput
        subTermInput
        mempty -- genTermInput
        parseTermInput
        prettyTermInput
        inferInput
        valueInput
        smallStepInput
        bigStepInput
