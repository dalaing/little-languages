module Term.Eval.SmallStep where

import Control.Lens (preview, review)
import Data.Foldable (asum)

import Term

eIfTrue :: Term n l a
        -> Maybe (Term n l a)
eIfTrue t = do
  (t1, t2, _) <- preview _TmIf t
  preview _TmTrue t1
  return t2

eIfFalse :: Term n l a
         -> Maybe (Term n l a)
eIfFalse t = do
  (t1, _, t3) <- preview _TmIf t
  preview _TmFalse t1
  return t3

eIf :: (Term n l a -> Maybe (Term n l a))
    -> Term n l a
    -> Maybe (Term n l a)
eIf step t = do
    (t1, t2, t3) <- preview _TmIf t
    u1 <- step t1
    return $ review _TmIf (u1, t2, t3)

eLoc :: Term n l a
     -> Maybe (Term n l a)
eLoc t = do
  (_, u) <- preview _TmLoc t
  return u

smallSteps :: [Term n l a -> Maybe (Term n l a)]
smallSteps = [
    eIfTrue
  , eIfFalse
  , eIf smallStep
  , eLoc
  ]

smallStep :: Term n l a
          -> Maybe (Term n l a)
smallStep t =
  asum .
  map ($ t) $
  smallSteps

sEval :: Term n l a
      -> Term n l a
sEval t = case smallStep t of
  Nothing -> t
  Just u -> sEval u
