module Term.Eval.SmallStep where

import Control.Lens (preview, review)
import Data.Foldable (asum)

import Term
import Term.Eval.Value

ePredZero :: Term l
          -> Maybe (Term l)
ePredZero t = do
  u <- preview _TmPred t
  preview _TmZero u
  return $ review _TmZero ()

ePredSucc :: Term l
          -> Maybe (Term l)
ePredSucc t = do
  u <- preview _TmPred t
  v <- preview _TmSucc u
  value v

ePred :: (Term l -> Maybe (Term l))
      -> Term l
      -> Maybe (Term l)
ePred step t = do
  u <- preview _TmPred t
  v <- step u
  return $ review _TmPred v

eSucc :: (Term l -> Maybe (Term l))
      -> Term l
      -> Maybe (Term l)
eSucc step t = do
  u <- preview _TmSucc t
  v <- step u
  return $ review _TmSucc v

eIfTrue :: Term l
        -> Maybe (Term l)
eIfTrue t = do
  (t1, t2, _) <- preview _TmIf t
  preview _TmTrue t1
  return t2

eIfFalse :: Term l
         -> Maybe (Term l)
eIfFalse t = do
  (t1, _, t3) <- preview _TmIf t
  preview _TmFalse t1
  return t3

eIf :: (Term l -> Maybe (Term l))
    -> Term l
    -> Maybe (Term l)
eIf step t = do
    (t1, t2, t3) <- preview _TmIf t
    u1 <- step t1
    return $ review _TmIf (u1, t2, t3)

eIsZeroZero :: Term l
            -> Maybe (Term l)
eIsZeroZero t = do
  u <- preview _TmIsZero t
  _ <- preview _TmZero u
  return $ review _TmTrue ()

eIsZeroSucc :: Term l
            -> Maybe (Term l)
eIsZeroSucc t = do
  u <- preview _TmIsZero t
  v <- preview _TmSucc u
  _ <- nv v
  return $ review _TmFalse ()

eIsZero :: (Term l -> Maybe (Term l))
        -> Term l
        -> Maybe (Term l)
eIsZero step t = do
  u <- preview _TmIsZero t
  v <- step u
  return $ review _TmIsZero v

eLoc :: Term l
     -> Maybe (Term l)
eLoc t = do
  (_, u) <- preview _TmLoc t
  return u

smallSteps :: [Term l -> Maybe (Term l)]
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
  , eLoc 
  ]

smallStep :: Term l -> Maybe (Term l)
smallStep t =
  asum .
  map ($ t) $
  smallSteps

sEval :: Term l -> Term l
sEval t = case smallStep t of
  Nothing -> t
  Just u -> sEval u

