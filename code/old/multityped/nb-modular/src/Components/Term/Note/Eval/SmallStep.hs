module Components.Term.Note.Eval.SmallStep where

import Control.Lens (preview)

import Common.Recursion (Step(..), stepFnToReader)
import Common.Term.Eval.SmallStep

import Components.Term.Note.Data

eNoted :: WithNoteTerm n ty tm
        => (tm -> Maybe tm)
        -> tm
        -> Maybe tm
eNoted step tm = do
  (_, t) <- preview _TmNoted tm
  step t

smallStepInput :: WithNoteTerm n ty tm
               => SmallStepInput tm
smallStepInput =
  SmallStepInput .
  fmap stepFnToReader $
    [ SRecurse eNoted ]
