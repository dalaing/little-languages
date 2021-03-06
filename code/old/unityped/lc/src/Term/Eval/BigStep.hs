module Term.Eval.BigStep where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Bound

import Term
import Term.Eval.Value (value)

eApp :: (Term n a -> Maybe (Term n a))
     -> Term n a
     -> Maybe (Term n a)
eApp step t = do
  (f, x) <- preview _TmApp t
  f' <- step f
  (_, e) <- preview _TmLam f'
  step $ instantiate1 x e

bigStepRules :: [Term n a -> Maybe (Term n a)]
bigStepRules =
  [ value
  , eApp bigStep
  ]

bigStep :: Term n a
        -> Maybe (Term n a)
bigStep t =
  asum .
  map ($ t) $
  bigStepRules

bEval :: Term n a
      -> Term n a
bEval t =
  fromMaybe t .
  bigStep $
  t

