module Term.Eval.SmallStep where

import Control.Lens (preview)
import Data.Foldable (asum)

import Term

eAddIntInt :: Term
           -> Maybe Term
eAddIntInt t = do
  (x, y) <- preview _TmAdd t
  x' <- preview _TmInt x
  y' <- preview _TmInt y
  return $ TmInt (x' + y')

eAdd1 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eAdd1 step t = do
  (x, y) <- preview _TmAdd t
  x' <- step x
  return $ TmAdd x' y

eAdd2 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eAdd2 step t = do
  (x, y) <- preview _TmAdd t
  _ <- preview _TmInt x
  y' <- step y
  return $ TmAdd x y'

eSubIntInt :: Term
           -> Maybe Term
eSubIntInt t = do
  (x, y) <- preview _TmSub t
  x' <- preview _TmInt x
  y' <- preview _TmInt y
  return $ TmInt (x' - y')

eSub1 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eSub1 step t = do
  (x, y) <- preview _TmSub t
  x' <- step x
  return $ TmSub x' y

eSub2 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eSub2 step t = do
  (x, y) <- preview _TmSub t
  _ <- preview _TmInt x
  y' <- step y
  return $ TmSub x y'

eMulIntInt :: Term
           -> Maybe Term
eMulIntInt t = do
  (x, y) <- preview _TmMul t
  x' <- preview _TmInt x
  y' <- preview _TmInt y
  return $ TmInt (x' * y')

eMul1 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eMul1 step t = do
  (x, y) <- preview _TmMul t
  x' <- step x
  return $ TmMul x' y

eMul2 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eMul2 step t = do
  (x, y) <- preview _TmMul t
  _ <- preview _TmInt x
  y' <- step y
  return $ TmMul x y'

eExpIntInt :: Term
           -> Maybe Term
eExpIntInt t = do
  (x, y) <- preview _TmExp t
  x' <- preview _TmInt x
  y' <- preview _TmInt y
  if y' >= 0
  then
    return $ TmInt (x' ^ y')
  else
    -- this is bad, and we should feel bad
    return $ TmInt 0

eExp1 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eExp1 step t = do
  (x, y) <- preview _TmExp t
  x' <- step x
  return $ TmExp x' y

eExp2 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eExp2 step t = do
  (x, y) <- preview _TmExp t
  _ <- preview _TmInt x
  y' <- step y
  return $ TmExp x y'

smallSteps :: [Term -> Maybe Term]
smallSteps =
  [ eAddIntInt
  , eAdd1 smallStep
  , eAdd2 smallStep
  , eSubIntInt
  , eSub1 smallStep
  , eSub2 smallStep
  , eMulIntInt
  , eMul1 smallStep
  , eMul2 smallStep
  , eExpIntInt
  , eExp1 smallStep
  , eExp2 smallStep
  ]

smallStep :: Term
          -> Maybe Term
smallStep t =
  asum .
  map ($ t) $
  smallSteps

sEval :: Term
      -> Term
sEval t = case smallStep t of
  Nothing -> t
  Just u -> sEval u

