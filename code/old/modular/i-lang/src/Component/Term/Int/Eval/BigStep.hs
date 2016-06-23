{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Int.Eval.BigStep (
    bigStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.BigStep (BigStepRule(..), BigStepInput(..))

import Component.Term.Int (AsIntTerm(..), WithIntTerm)

-- |
eInt :: WithIntTerm tm
     => tm nTy nTm a             -- ^
     -> Maybe (tm nTy nTm a)     -- ^
eInt =
  fmap (review _TmIntLit) .
  preview _TmIntLit

-- |
eAdd :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a
      -> Maybe (tm nTy nTm a)
eAdd step tm = do
  (tm1, tm2) <- preview _TmAdd tm
  tm1' <- step tm1
  i1 <- preview _TmIntLit tm1'
  tm2' <- step tm2
  i2 <- preview _TmIntLit tm2'
  return $ review _TmIntLit (i1 + i2)

-- |
eSub :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a
      -> Maybe (tm nTy nTm a)
eSub step tm = do
  (tm1, tm2) <- preview _TmSub tm
  tm1' <- step tm1
  i1 <- preview _TmIntLit tm1'
  tm2' <- step tm2
  i2 <- preview _TmIntLit tm2'
  return $ review _TmIntLit (i1 - i2)

-- |
eMul :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a
      -> Maybe (tm nTy nTm a)
eMul step tm = do
  (tm1, tm2) <- preview _TmMul tm
  tm1' <- step tm1
  i1 <- preview _TmIntLit tm1'
  tm2' <- step tm2
  i2 <- preview _TmIntLit tm2'
  return $ review _TmIntLit (i1 * i2)

-- |
eExp :: WithIntTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a
      -> Maybe (tm nTy nTm a)
eExp step tm = do
  (tm1, tm2) <- preview _TmExp tm
  tm1' <- step tm1
  i1 <- preview _TmIntLit tm1'
  tm2' <- step tm2
  i2 <- preview _TmIntLit tm2'
  return . review _TmIntLit $
    if i2 < 0
    then 0
    else i1 ^ i2

-- |
bigStepInput :: WithIntTerm tm
             => BigStepInput tm
bigStepInput =
  BigStepInput
    [ BigStepBase eInt
    , BigStepRecurse eAdd
    , BigStepRecurse eSub
    , BigStepRecurse eMul
    , BigStepRecurse eExp
    ]
