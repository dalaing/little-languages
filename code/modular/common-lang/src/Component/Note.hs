{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Note (
    noteRules
  ) where

import Common.Note (TranslateNote)
import           Component                           (ComponentInput (..))
import           Component.Term                      (TermInput (..))
import           Component.Type                      (TypeInput (..))
import           Component.Term.Parent              (AsParentTerm)
import           Component.Term.Note                (NoteTerm, WithNoteTerm)
import           Component.Term.Note.Strip          (stripNoteTermInput)
import           Component.Term.Note.Eval.BigStep   (bigStepInput)
import           Component.Term.Note.Eval.SmallStep (smallStepInput)
import           Component.Term.Note.Eval.Value     (valueInput)
import           Component.Term.Note.Gen            (genTermInput)
import           Component.Term.Note.Infer          (inferInput)
import           Component.Term.Note.Parse          (parseTermInput)
import           Component.Term.Note.Pretty         (prettyTermInput)
import           Component.Term.Note.Size           (termSizeInput)
import           Component.Type.Parent              (AsParentType)
import           Component.Type.Note                (NoteType, WithNoteType)
import           Component.Type.Note.Strip          (stripNoteTypeInput)
import           Component.Type.Note.Gen            (genTypeInput)
import           Component.Type.Note.Parse          (parseTypeInput)
import           Component.Type.Note.Pretty         (prettyTypeInput)

noteRules :: ( Eq (ty nTy)
             , WithNoteType ty nTy
             , WithNoteTerm tm nTm a
             , AsParentTerm (NoteTerm tm) tm
             , AsParentType (NoteType ty) ty
             , TranslateNote nTm nTy
             )
          => ComponentInput r e ty nTy tm nTm a
noteRules =
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
