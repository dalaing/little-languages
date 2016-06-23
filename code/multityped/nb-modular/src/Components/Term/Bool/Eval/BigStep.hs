module Components.Term.Bool.Eval.BigStep where

import Control.Lens (preview)

import Common.Recursion (Step(..), stepFnToReader)
import Common.Term.Eval.BigStep

import Components.Term.Bool.Data
import Components.Term.Bool.Eval.Value

eIfFalse :: WithBoolTerm ty tm
         => (tm -> Maybe tm)
         -> tm
         -> Maybe tm
eIfFalse step tm = do
    (t1, _, t3) <- preview _TmIf tm
    t1' <- step t1
    preview _TmFalse t1'
    step t3

eIfTrue :: WithBoolTerm ty tm
         => (tm -> Maybe tm)
         -> tm
         -> Maybe tm
eIfTrue step tm = do
  (t1, t2, _) <- preview _TmIf tm
  t1' <- step t1
  preview _TmTrue t1'
  step t2

bigStepInput :: WithBoolTerm ty tm
             => BigStepInput tm
bigStepInput =
  BigStepInput .
  fmap stepFnToReader $
    [ SBase bv
    , SRecurse eIfFalse
    , SRecurse eIfTrue
    ]
