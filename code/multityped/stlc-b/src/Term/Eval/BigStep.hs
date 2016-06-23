module Term.Eval.BigStep where

import Control.Lens (preview, review)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Term
import Term.Eval.Value (bv)

eIfTrue :: (Term n l a -> Maybe (Term n l a))
        -> Term n l a
        -> Maybe (Term n l a)
eIfTrue step t = do
    (t1, t2, _) <- preview _TmIf t
    u1 <- step t1
    preview _TmTrue u1
    step t2

eIfFalse :: (Term n l a -> Maybe (Term n l a))
         -> Term n l a
         -> Maybe (Term n l a)
eIfFalse step t = do
    (t1, _, t3) <- preview _TmIf t
    u1 <- step t1
    preview _TmFalse u1
    step t3

eLoc :: (Term n l a -> Maybe (Term n l a))
     -> Term n l a
     -> Maybe (Term n l a)
eLoc step t = do
  (l, u) <- preview _TmLoc t
  v <- step u
  return $ review _TmLoc (l, v)

bigSteps :: [Term n l a -> Maybe (Term n l a)]
bigSteps =
  [ bv
  , eIfTrue bigStep
  , eIfFalse bigStep
  , eLoc bigStep
  ]

bigStep :: Term n l a
        -> Maybe (Term n l a)
bigStep t =
  asum .
  map ($ t) $
  bigSteps

bEval :: Term n l a
      -> Term n l a
bEval t =
  fromMaybe t .
  bigStep $
  t

