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

import           Component                           (ComponentInput (..))
import           Component.Term                      (TermInput (..))
import           Component.Type                      (TypeInput (..))
import Component.Type.Note (WithNoteType)
import           Component.Type.Error.Unexpected        (AsUnexpected)

import           Component.Term.STLC                (WithSTLCTerm)
import           Component.Term.STLC.Size           (termSizeInput)
-- import           Component.Term.STLC.Strip           (stripNoteTermInput)
import           Component.Term.STLC.Eval.Value     (valueInput)
import           Component.Term.STLC.Eval.SmallStep (smallStepInput)
import           Component.Term.STLC.Eval.BigStep   (bigStepInput)
-- import           Component.Term.STLC.Gen            (genTermInput)
import           Component.Term.STLC.Infer          (inferInput)
import           Component.Term.STLC.Parse          (parseTermInput)
import           Component.Term.STLC.Pretty         (prettyTermInput)
import           Component.Type.STLC                (WithSTLCType, HasContext)
-- import           Component.Type.STLC.Strip           (stripNoteTypeInput)
-- import           Component.Type.STLC.Gen            (genTypeInput)
import           Component.Type.STLC.Parse          (parseTypeInput)
import           Component.Type.STLC.Pretty         (prettyTypeInput)
import           Component.Type.Error.FreeVar       (freeVarInput, AsFreeVar)
import           Component.Type.Error.NotArrow      (notArrowInput, notArrowSrcLocInput, AsNotArrow)

stlcErrors :: ( AsFreeVar e String
              , AsNotArrow e ty nTy
              )
           => ComponentInput r e ty nTy tm nTm String
stlcErrors =
  ComponentInput
    mempty
    (mappend freeVarInput notArrowInput)
    mempty

stlcSrcLocErrors :: ( AsFreeVar e String
                    , AsNotArrow e ty nTy
                    , WithNoteType ty nTy
                    , Renderable nTy
                    )
                 => ComponentInput r e ty nTy tm nTm String
stlcSrcLocErrors =
  ComponentInput
    mempty
    (mappend freeVarInput notArrowSrcLocInput)
    mempty

stlcRules :: ( Eq (ty nTy)
             , AsUnexpected e ty nTy
             , AsNotArrow e ty nTy
             , AsFreeVar e String
             , HasContext r ty nTy String
             , WithSTLCType ty nTy
             , WithSTLCTerm tm ty nTy nTm String
             , Monad (tm nTm)
             )
          => ComponentInput r e ty nTy tm nTm String
stlcRules =
    ComponentInput tyI mempty tmI
  where
    tyI =
      TypeInput
        mempty -- stripNoteTypeInput
        mempty -- genTypeInput
        parseTypeInput
        prettyTypeInput
    tmI =
      TermInput
        termSizeInput
        mempty -- stripNoteTermInput
        mempty -- genTermInput
        parseTermInput
        prettyTermInput
        inferInput
        valueInput
        smallStepInput
        bigStepInput
