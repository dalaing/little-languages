{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.NatBool.Eval.BigStep (
    bigStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.BigStep (BigStepRule(..), BigStepInput(..))

import Component.Term.Nat (AsNatTerm(..))
import Component.Term.Bool (AsBoolTerm(..))
import Component.Term.NatBool (AsNatBoolTerm(..), WithNatBoolTerm)

-- |
eIsZeroZero :: WithNatBoolTerm tm
            => (tm nTy nTm a -> Maybe (tm nTy nTm a))
            -> tm nTy nTm a
            -> Maybe (tm nTy nTm a)
eIsZeroZero step tm = do
  t <- preview _TmIsZero tm
  t' <- step t
  _ <- preview _TmZero t'
  return $ review _TmTrue ()

-- |
eIsZeroSucc :: WithNatBoolTerm tm
            => (tm nTy nTm a -> Maybe (tm nTy nTm a))
            -> tm nTy nTm a
            -> Maybe (tm nTy nTm a)
eIsZeroSucc step tm = do
  t <- preview _TmIsZero tm
  t' <- step t
  _ <- preview _TmSucc t'
  return $ review _TmFalse ()

-- |
bigStepInput :: WithNatBoolTerm tm
             => BigStepInput tm
bigStepInput =
  BigStepInput
    [ BigStepRecurse eIsZeroZero
    , BigStepRecurse eIsZeroSucc
    ]
