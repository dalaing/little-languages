module Term.Eval.SmallStep where

import Control.Lens (preview)
import Data.Foldable (asum)

import Term

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

smallSteps :: [Term -> Maybe Term]
smallSteps =
  [ eIfTrue
  , eIfFalse
  , eIf smallStep
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

