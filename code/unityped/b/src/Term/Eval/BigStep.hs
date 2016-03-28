module Term.Eval.BigStep where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Term
import Term.Eval.Value (value)

eIfTrue :: (Term -> Maybe Term)
        -> Term
        -> Maybe Term
eIfTrue step t = do
    (t1, t2, _) <- preview _TmIf t
    u1 <- step t1
    preview _TmTrue u1
    step t2

eIfFalse :: (Term -> Maybe Term)
         -> Term
         -> Maybe Term
eIfFalse step t = do
    (t1, _, t3) <- preview _TmIf t
    u1 <- step t1
    preview _TmFalse u1
    step t3

bigSteps :: [Term -> Maybe Term]
bigSteps =
  [ value
  , eIfTrue bigStep
  , eIfFalse bigStep
  ]

bigStep :: Term
        -> Maybe Term
bigStep t =
  asum .
  map ($ t) $
  bigSteps

bEval :: Term
      -> Term
bEval t =
  fromMaybe t .
  bigStep $
  t

