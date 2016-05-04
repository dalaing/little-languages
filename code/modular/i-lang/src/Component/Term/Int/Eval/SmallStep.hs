{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Int.Eval.SmallStep (
    smallStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.SmallStep (SmallStepRule(..), SmallStepInput(..))

import Component.Term.Int (AsIntTerm(..), WithIntTerm)

-- |
eAddIntInt :: WithIntTerm tm
           => tm nTy nTm a             -- ^
           -> Maybe (tm nTy nTm a)     -- ^
eAddIntInt tm = do
  (tm1, tm2) <- preview _TmAdd tm
  i1 <- preview _TmIntLit tm1
  i2 <- preview _TmIntLit tm2
  return $ review _TmIntLit (i1 + i2)

-- |
eAdd1 :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a             -- ^
      -> Maybe (tm nTy nTm a)     -- ^
eAdd1 step tm = do
  (tm1, tm2) <- preview _TmAdd tm
  tm1' <- step tm1
  return $ review _TmAdd (tm1', tm2)

-- |
eAdd2 :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> tm nTy nTm a                     -- ^
      -> Maybe (tm nTy nTm a)             -- ^
eAdd2 value step tm = do
  (tm1, tm2) <- preview _TmAdd tm
  _ <- value tm1
  tm2' <- step tm2
  return $ review _TmAdd (tm1, tm2')

-- |
eSubIntInt :: WithIntTerm tm
           => tm nTy nTm a             -- ^
           -> Maybe (tm nTy nTm a)     -- ^
eSubIntInt tm = do
  (tm1, tm2) <- preview _TmSub tm
  i1 <- preview _TmIntLit tm1
  i2 <- preview _TmIntLit tm2
  return $ review _TmIntLit (i1 - i2)

-- |
eSub1 :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a             -- ^
      -> Maybe (tm nTy nTm a)     -- ^
eSub1 step tm = do
  (tm1, tm2) <- preview _TmSub tm
  tm1' <- step tm1
  return $ review _TmSub (tm1', tm2)

-- |
eSub2 :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> tm nTy nTm a                     -- ^
      -> Maybe (tm nTy nTm a)             -- ^
eSub2 value step tm = do
  (tm1, tm2) <- preview _TmSub tm
  _ <- value tm1
  tm2' <- step tm2
  return $ review _TmSub (tm1, tm2')

-- |
eMulIntInt :: WithIntTerm tm
           => tm nTy nTm a             -- ^
           -> Maybe (tm nTy nTm a)     -- ^
eMulIntInt tm = do
  (tm1, tm2) <- preview _TmMul tm
  i1 <- preview _TmIntLit tm1
  i2 <- preview _TmIntLit tm2
  return $ review _TmIntLit (i1 * i2)

-- |
eMul1 :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a             -- ^
      -> Maybe (tm nTy nTm a)     -- ^
eMul1 step tm = do
  (tm1, tm2) <- preview _TmMul tm
  tm1' <- step tm1
  return $ review _TmMul (tm1', tm2)

-- |
eMul2 :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> tm nTy nTm a                     -- ^
      -> Maybe (tm nTy nTm a)             -- ^
eMul2 value step tm = do
  (tm1, tm2) <- preview _TmMul tm
  _ <- value tm1
  tm2' <- step tm2
  return $ review _TmMul (tm1, tm2')

-- |
eExpIntInt :: WithIntTerm tm
           => tm nTy nTm a             -- ^
           -> Maybe (tm nTy nTm a)     -- ^
eExpIntInt tm = do
  (tm1, tm2) <- preview _TmExp tm
  i1 <- preview _TmIntLit tm1
  i2 <- preview _TmIntLit tm2
  return . review _TmIntLit $
    if i2 < 0
    then 0
    else i1 ^ i2

-- |
eExp1 :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a             -- ^
      -> Maybe (tm nTy nTm a)     -- ^
eExp1 step tm = do
  (tm1, tm2) <- preview _TmExp tm
  tm1' <- step tm1
  return $ review _TmExp (tm1', tm2)

-- |
eExp2 :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> (tm nTy nTm a -> Maybe (tm nTy nTm a)) -- ^
      -> tm nTy nTm a                     -- ^
      -> Maybe (tm nTy nTm a)             -- ^
eExp2 value step tm = do
  (tm1, tm2) <- preview _TmExp tm
  _ <- value tm1
  tm2' <- step tm2
  return $ review _TmExp (tm1, tm2')

-- |
smallStepInput :: WithIntTerm tm
               => SmallStepInput tm nTy nTm a
smallStepInput =
  SmallStepInput
    [ SmallStepBase eAddIntInt
    , SmallStepRecurse eAdd1
    , SmallStepValueRecurse eAdd2
    , SmallStepBase eSubIntInt
    , SmallStepRecurse eSub1
    , SmallStepValueRecurse eSub2
    , SmallStepBase eMulIntInt
    , SmallStepRecurse eMul1
    , SmallStepValueRecurse eMul2
    , SmallStepBase eExpIntInt
    , SmallStepRecurse eExp1
    , SmallStepValueRecurse eExp2
    ]
