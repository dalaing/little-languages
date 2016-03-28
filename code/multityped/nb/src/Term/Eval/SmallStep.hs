module Term.Eval.SmallStep where

import Control.Lens (preview, review)
import Data.Foldable (asum)

import Term
import Term.Eval.Value

ePredZero :: Term
          -> Maybe Term
ePredZero t = do
  u <- preview _TmPred t
  preview _TmZero u
  return TmZero

ePredSucc :: Term
          -> Maybe Term
ePredSucc t = do
  u <- preview _TmPred t
  v <- preview _TmSucc u
  value v

ePred :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
ePred step t = do
  u <- preview _TmPred t
  v <- step u
  return $ review _TmPred v

eSucc :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eSucc step t = do
  u <- preview _TmSucc t
  v <- step u
  return $ TmSucc v

eIfTrue :: Term
        -> Maybe Term
eIfTrue t = do
  (t1, t2, _) <- preview _TmIf t
  preview _TmTrue t1
  return t2

eIfFalse :: Term
         -> Maybe Term
eIfFalse t = do
  (t1, _, t3) <- preview _TmIf t
  preview _TmFalse t1
  return t3

eIf :: (Term -> Maybe Term)
    -> Term
    -> Maybe Term
eIf step t = do
    (t1, t2, t3) <- preview _TmIf t
    u1 <- step t1
    return $ TmIf u1 t2 t3

eIsZeroZero :: Term
            -> Maybe Term
eIsZeroZero t = do
  u <- preview _TmIsZero t
  _ <- preview _TmZero u
  return TmTrue

eIsZeroSucc :: Term
            -> Maybe Term
eIsZeroSucc t = do
  u <- preview _TmIsZero t
  v <- preview _TmSucc u
  _ <- nv v
  return TmFalse

eIsZero :: (Term -> Maybe Term)
        -> Term
        -> Maybe Term
eIsZero step t = do
  u <- preview _TmIsZero t
  v <- step u
  return $ TmIsZero v

smallSteps :: [Term -> Maybe Term]
smallSteps = [
    ePredZero
  , ePredSucc
  , ePred smallStep
  , eSucc smallStep
  , eIfTrue
  , eIfFalse
  , eIf smallStep
  , eIsZeroZero
  , eIsZeroSucc
  , eIsZero smallStep
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

