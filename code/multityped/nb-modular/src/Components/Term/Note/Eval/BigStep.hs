module Components.Term.Note.Eval.BigStep where

import Control.Lens (preview)

import Common.Recursion (Step(..), stepFnToReader)
import Common.Term.Eval.BigStep

import Components.Term.Note.Data

eNoted :: WithNoteTerm n ty tm
       => (tm -> Maybe tm)
       -> tm
       -> Maybe tm
eNoted step tm = do
  (_, t) <- preview _TmNoted tm
  step t

bigStepInput :: WithNoteTerm n ty tm
             => BigStepInput tm
bigStepInput =
  BigStepInput .
  fmap stepFnToReader $
    [ SRecurse eNoted ]
