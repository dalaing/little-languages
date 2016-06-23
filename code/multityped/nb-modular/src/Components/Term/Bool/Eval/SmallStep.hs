module Components.Term.Bool.Eval.SmallStep where

import Control.Lens (preview, review)

import Common.Recursion (Step(..), stepFnToReader)
import Common.Term.Eval.SmallStep

import Components.Term.Bool.Data

eIfFalse :: WithBoolTerm ty tm
        => tm
        -> Maybe tm
eIfFalse tm = do
  (t1, _, t3) <- preview _TmIf tm
  _ <- preview _TmFalse t1
  return t3

eIfTrue :: WithBoolTerm ty tm
        => tm
        -> Maybe tm
eIfTrue tm = do
  (t1, t2, _) <- preview _TmIf tm
  _ <- preview _TmTrue t1
  return t2

eIf :: WithBoolTerm ty tm
    => (tm -> Maybe tm)
    -> tm
    -> Maybe tm
eIf step tm = do
  (t1, t2, t3) <- preview _TmIf tm
  t1' <- step t1
  return $ review _TmIf (t1', t2, t3)

smallStepInput :: WithBoolTerm ty tm
               => SmallStepInput tm
smallStepInput =
  SmallStepInput .
  fmap stepFnToReader $
    [ SBase eIfFalse
    , SBase eIfTrue
    , SRecurse eIf
    ]
