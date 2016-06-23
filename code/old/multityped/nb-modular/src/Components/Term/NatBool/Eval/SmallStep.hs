module Components.Term.NatBool.Eval.SmallStep where

import Control.Lens (preview, review)

import Common.Recursion (Step(..), stepFnToReader)
import Common.Term.Eval.SmallStep

import Components.Term.Nat.Data
import Components.Term.Nat.Eval.Value (nv)
import Components.Term.Bool.Data
import Components.Term.NatBool.Data

eIsZeroZero :: ( WithNatBoolTerm ty tm
               , WithNatTerm ty tm
               , WithBoolTerm ty tm
               )
            => tm
            -> Maybe tm
eIsZeroZero tm = do
  t <- preview _TmIsZero tm
  _ <- preview _TmZero t
  return $ review _TmTrue ()

eIsZeroSucc :: ( WithNatBoolTerm ty tm
               , WithNatTerm ty tm
               , WithBoolTerm ty tm
               )
            => tm
            -> Maybe tm
eIsZeroSucc tm = do
  t <- preview _TmIsZero tm
  t' <- preview _TmSucc t
  _ <- nv t'
  return $ review _TmFalse ()

eIsZero :: WithNatBoolTerm ty tm
        => (tm -> Maybe tm)
        -> tm
        -> Maybe tm
eIsZero step tm = do
  t <- preview _TmIsZero tm
  t' <- step t
  return $ review _TmIsZero t'

smallStepInput :: ( WithNatBoolTerm ty tm
                  , WithNatTerm ty tm
                  , WithBoolTerm ty tm
                  )
               => SmallStepInput tm
smallStepInput =
  SmallStepInput .
  fmap stepFnToReader $
    [ SBase eIsZeroZero
    , SBase eIsZeroSucc
    , SRecurse eIsZero
    ]
