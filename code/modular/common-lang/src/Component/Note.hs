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
import           Component                          (ComponentInput (..))
import           Component.Term                     (TermInput (..))
import           Component.Type                     (TypeInput (..))
import           Component.Term.Note                (WithNoteTerm)
import           Component.Term.Note.Eval.BigStep   (bigStepInput)
import           Component.Term.Note.Eval.SmallStep (smallStepInput)
import           Component.Term.Note.Eval.Value     (valueInput)
import           Component.Term.Note.Gen            (genTermInput)
import           Component.Term.Note.Infer          (inferInput)
import           Component.Term.Note.Parse          (parseTermInput)
import           Component.Term.Note.Pretty         (prettyTermInput)
import           Component.Term.Note.SubTerm        (subTermInput)
import           Component.Type.Note                (WithNoteType)
import           Component.Type.Note.Gen            (genTypeInput)
import           Component.Type.Note.Parse          (parseTypeInput)
import           Component.Type.Note.Pretty         (prettyTypeInput)
import Extras (Eq1)

noteRules :: ( Eq1 ty
             , WithNoteType ty
             , WithNoteTerm tm
             -- , TranslateNote nTm nTy
             )
          => ComponentInput r e ty tm
noteRules =
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
