{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.NatBool.Eval.SmallStep (
    smallStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.SmallStep (SmallStepRule(..), SmallStepInput(..))

import Component.Term.Nat (AsNatTerm(..))
import Component.Term.Bool (AsBoolTerm(..))
import Component.Term.NatBool (AsNatBoolTerm(..), WithNatBoolTerm)

-- |
eIsZeroZero :: WithNatBoolTerm tm n a
            => tm n a             -- ^
            -> Maybe (tm n a)        -- ^
eIsZeroZero tm = do
  tm1 <- preview _TmIsZero tm
  _ <- preview _TmZero tm1
  return $ review _TmTrue ()

-- |
eIsZeroSucc :: WithNatBoolTerm tm n a
            => (tm n a -> Maybe (tm n a)) -- ^
            -> tm n a                     -- ^
            -> Maybe (tm n a)             -- ^
eIsZeroSucc nv tm = do
  tm1 <- preview _TmIsZero tm
  tm2 <- preview _TmSucc tm1
  _ <- nv tm2
  return $ review _TmFalse ()

-- |
eIsZero :: WithNatBoolTerm tm n a
        => (tm n a -> Maybe (tm n a)) -- ^
        -> tm n a                     -- ^
        -> Maybe (tm n a)             -- ^
eIsZero step tm = do
  tm1 <- preview _TmIsZero tm
  tm2 <- step tm1
  return $ review _TmIsZero tm2

-- |
smallStepInput :: WithNatBoolTerm tm n a
               => SmallStepInput tm n a
smallStepInput =
  SmallStepInput
    [ SmallStepBase eIsZeroZero
    , SmallStepValue eIsZeroSucc
    , SmallStepRecurse eIsZero
    ]
