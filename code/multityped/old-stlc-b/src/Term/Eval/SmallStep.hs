module Term.Eval.SmallStep where

import Control.Lens (preview, review)
import Data.Foldable (asum)

import Term
import Term.Eval.Value

eIfTrue :: Term l n a -> Maybe (Term l n a)
eIfTrue t = do
  (t1, t2, _) <- preview _TmIf t
  preview _TmTrue t1
  return t2

eIfFalse :: Term l n a -> Maybe (Term l n a)
eIfFalse t = do
  (t1, _, t3) <- preview _TmIf t
  preview _TmFalse t1
  return t3

eIf :: (Term l n a -> Maybe (Term l n a)) -> Term l n a -> Maybe (Term l n a)
eIf step t = do
    (t1, t2, t3) <- preview _TmIf t
    u1 <- step t1
    return $ review _TmIf (u1, t2, t3)

eLoc :: (Term l n a -> Maybe (Term l n a)) -> Term l n a -> Maybe (Term l n a)
eLoc step t = do
  (l, u) <- preview _TmLoc t
  v <- step u
  return $ review _TmLoc (l, v)

smallSteps :: [Term l n a -> Maybe (Term l n a)]
smallSteps = [
    eIfTrue
  , eIfFalse
  , eIf smallStep
  , eLoc smallStep
  ]

smallStep :: Term l n a -> Maybe (Term l n a)
smallStep t =
  asum .
  map ($ t) $
  smallSteps

sEval :: Term l n a -> Term l n a
sEval t = case smallStep t of
  Nothing -> t
  Just u -> sEval u

