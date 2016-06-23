module Term.Eval.BigStep where

import Control.Lens (preview, review)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Term
import Term.Eval.Value (value)

eIfTrue :: (Term l n a -> Maybe (Term l n a))
        -> Term l n a
        -> Maybe (Term l n a)
eIfTrue step t = do
    (t1, t2, _) <- preview _TmIf t
    u1 <- step t1
    preview _TmTrue u1
    step t2

eIfFalse :: (Term l n a -> Maybe (Term l n a))
         -> Term l n a
         -> Maybe (Term l n a)
eIfFalse step t = do
    (t1, _, t3) <- preview _TmIf t
    u1 <- step t1
    preview _TmFalse u1
    step t3

eLoc :: (Term l n a -> Maybe (Term l n a))
     -> Term l n a
     -> Maybe (Term l n a)
eLoc step t = do
  (l, u) <- preview _TmLoc t
  v <- step u
  return $ review _TmLoc (l, v)

bigSteps :: [Term l n a -> Maybe (Term l n a)]
bigSteps = [
    value
  , eIfTrue bigStep
  , eIfFalse bigStep
  , eLoc bigStep
  ]

bigStep :: Term l n a -> Maybe (Term l n a)
bigStep t =
  asum .
  map ($ t) $
  bigSteps

bEval :: Term l n a -> Term l n a
bEval t = fromMaybe t . bigStep $ t

