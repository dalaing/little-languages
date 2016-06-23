module Components.Term.Nat.Eval.BigStep where

import Control.Lens (preview, review)

import Common.Recursion (Step(..), stepFnToReader)
import Common.Term.Eval.BigStep

import Components.Term.Nat.Data

eZero :: WithNatTerm ty tm
      => tm
      -> Maybe tm
eZero tm = do
  _ <- preview _TmZero tm
  return $ review _TmZero ()

eSucc :: WithNatTerm ty tm
      => (tm -> Maybe tm)
      -> tm
      -> Maybe tm
eSucc step tm = do
  t <- preview _TmSucc tm
  t' <- step t
  return $ review _TmSucc t'

ePredZero :: WithNatTerm ty tm
          => (tm -> Maybe tm)
          -> tm
          -> Maybe tm
ePredZero step tm = do
  t <- preview _TmPred tm
  t' <- step t
  preview _TmZero t'
  return $ review _TmZero ()

ePredSucc :: WithNatTerm ty tm
          => (tm -> Maybe tm)
          -> tm
          -> Maybe tm
ePredSucc step tm = do
  t <- preview _TmPred tm
  t' <- step t
  preview _TmSucc t'

bigStepInput :: WithNatTerm ty tm
             => BigStepInput tm
bigStepInput =
  BigStepInput .
  fmap stepFnToReader $
    [ SBase eZero
    , SRecurse eSucc
    , SRecurse ePredZero
    , SRecurse ePredSucc
    ]
