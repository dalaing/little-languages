module Components.Term.NatBool.Eval.BigStep where

import Control.Lens (preview, review)

import Common.Recursion (Step(..), stepFnToReader)
import Common.Term.Eval.BigStep

import Components.Term.Nat.Data
import Components.Term.Bool.Data
import Components.Term.NatBool.Data

eIsZeroZero :: ( WithNatBoolTerm ty tm
               , WithNatTerm ty tm
               , WithBoolTerm ty tm
               )
            => (tm -> Maybe tm)
            -> tm
            -> Maybe tm
eIsZeroZero step tm = do
  t <- preview _TmIsZero tm
  t' <- step t
  _ <- preview _TmZero t'
  return $ review _TmTrue ()

eIsZeroSucc :: ( WithNatBoolTerm ty tm
               , WithNatTerm ty tm
               , WithBoolTerm ty tm
               )
            => (tm -> Maybe tm)
            -> tm
            -> Maybe tm
eIsZeroSucc step tm = do
  t <- preview _TmIsZero tm
  t' <- step t
  _ <- preview _TmSucc t'
  return $ review _TmFalse ()

bigStepInput :: ( WithNatBoolTerm ty tm
                , WithNatTerm ty tm
                , WithBoolTerm ty tm
                )
             => BigStepInput tm
bigStepInput =
  BigStepInput .
  fmap stepFnToReader $
    [ SRecurse eIsZeroZero
    , SRecurse eIsZeroSucc
    ]
