module Components.Term.Nat.Eval.SmallStep where

import Control.Lens (preview, review)

import Common.Recursion (Step(..), stepFnToReader)
import Common.Term.Eval.SmallStep

import Components.Term.Nat.Data
import Components.Term.Nat.Eval.Value (nv)

ePredZero :: WithNatTerm ty tm
        => tm
        -> Maybe tm
ePredZero tm = do
  t <- preview _TmPred tm
  _ <- preview _TmZero t
  return $ review _TmZero ()

ePredSucc :: WithNatTerm ty tm
        => tm
        -> Maybe tm
ePredSucc tm = do
  t <- preview _TmPred tm
  t' <- preview _TmSucc t
  nv t'

ePred :: WithNatTerm ty tm
    => (tm -> Maybe tm)
    -> tm
    -> Maybe tm
ePred step tm = do
  t <- preview _TmPred tm
  t' <- step t
  return $ review _TmPred t'

eSucc :: WithNatTerm ty tm
    => (tm -> Maybe tm)
    -> tm
    -> Maybe tm
eSucc step tm = do
  t <- preview _TmSucc tm
  t' <- step t
  return $ review _TmSucc t'

smallStepInput :: WithNatTerm ty tm
               => SmallStepInput tm
smallStepInput =
  SmallStepInput .
  fmap stepFnToReader $
    [ SBase ePredZero
    , SBase ePredSucc
    , SRecurse ePred
    , SRecurse eSucc
    ]
