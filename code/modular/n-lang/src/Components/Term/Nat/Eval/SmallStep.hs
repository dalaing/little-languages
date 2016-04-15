{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Nat.Eval.SmallStep (
    smallStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.SmallStep (SmallStepRule(..), SmallStepInput(..))

import Components.Term.Nat (AsNatTerm(..), WithNatTerm)

-- |
ePredZero :: WithNatTerm tm a
          => tm a             -- ^
          -> Maybe (tm a)        -- ^
ePredZero tm = do
  tm1 <- preview _TmPred tm
  _ <- preview _TmZero tm1
  return $ review _TmZero ()

-- |
ePredSucc :: WithNatTerm tm a
          => (tm a -> Maybe (tm a)) -- ^
          -> tm a              -- ^
          -> Maybe (tm a)         -- ^
ePredSucc nv tm = do
  tm1 <- preview _TmPred tm
  tm1' <- preview _TmSucc tm1
  nv tm1'

-- |
eSucc :: WithNatTerm tm a
      => (tm a -> Maybe (tm a)) -- ^
      -> tm a              -- ^
      -> Maybe (tm a)         -- ^
eSucc step tm = do
  tm1 <- preview _TmSucc tm
  tm1' <- step tm1
  return $ review _TmSucc tm1'

-- |
ePred :: WithNatTerm tm a
      => (tm a -> Maybe (tm a)) -- ^
      -> tm a              -- ^
      -> Maybe (tm a)         -- ^
ePred step tm = do
  tm1 <- preview _TmPred tm
  tm1' <- step tm1
  return $ review _TmPred tm1'

-- |
smallStepInput :: WithNatTerm tm a
               => SmallStepInput (tm a)
smallStepInput =
  SmallStepInput
    [ SmallStepBase ePredZero
    , SmallStepValue ePredSucc
    , SmallStepRecurse eSucc
    , SmallStepRecurse ePred
    ]
