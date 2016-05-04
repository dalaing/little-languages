{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Nat.Eval.SmallStep (
    smallStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.SmallStep (SmallStepRule(..), SmallStepInput(..))

import Component.Term.Nat (AsNatTerm(..), WithNatTerm)

-- |
ePredZero :: WithNatTerm tm
          => tm nTy nTm a             -- ^
          -> Maybe (tm nTy nTm a)        -- ^
ePredZero tm = do
  tm1 <- preview _TmPred tm
  _ <- preview _TmZero tm1
  return $ review _TmZero ()

-- |
ePredSucc :: WithNatTerm tm
          => (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
          -> tm nTy nTm a              -- ^
          -> Maybe (tm nTy nTm a)         -- ^
ePredSucc nv tm = do
  tm1 <- preview _TmPred tm
  tm1' <- preview _TmSucc tm1
  nv tm1'

-- |
eSucc :: WithNatTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> tm nTy nTm a              -- ^
      -> Maybe (tm nTy nTm a)         -- ^
eSucc step tm = do
  tm1 <- preview _TmSucc tm
  tm1' <- step tm1
  return $ review _TmSucc tm1'

-- |
ePred :: WithNatTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> tm nTy nTm a              -- ^
      -> Maybe (tm nTy nTm a)         -- ^
ePred step tm = do
  tm1 <- preview _TmPred tm
  tm1' <- step tm1
  return $ review _TmPred tm1'

-- |
smallStepInput :: WithNatTerm tm
               => SmallStepInput tm nTy nTm a
smallStepInput =
  SmallStepInput
    [ SmallStepBase ePredZero
    , SmallStepValue ePredSucc
    , SmallStepRecurse eSucc
    , SmallStepRecurse ePred
    ]
