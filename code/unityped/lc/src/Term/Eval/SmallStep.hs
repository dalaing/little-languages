module Term.Eval.SmallStep where

import Control.Lens (preview)
import Data.Foldable (asum)

import Bound

import Term
import Term.Eval.Value

eAppLam :: Term n a -> Maybe (Term n a)
eAppLam t = do
  (f, x) <- preview _TmApp t
  (_, e) <- preview _TmLam f
  return $ instantiate1 x e

eAppLamStrict :: Term n a -> Maybe (Term n a)
eAppLamStrict t = do
  (f, x) <- preview _TmApp t
  _ <- value x
  (_, e) <- preview _TmLam f
  return $ instantiate1 x e

eApp1 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
eApp1 step t = do
  (x, y) <- preview _TmApp t
  x' <- step x
  return $ TmApp x' y

eApp2 :: (Term n a -> Maybe (Term n a)) -> Term n a -> Maybe (Term n a)
eApp2 step t = do
  (x, y) <- preview _TmApp t
  _ <- value x
  y' <- step y
  return $ TmApp x y'

smallSteps :: [Term n a -> Maybe (Term n a)]
smallSteps = [
    eAppLam
  , eApp1 smallStep
  ]

smallStepsStrict :: [Term n a -> Maybe (Term n a)]
smallStepsStrict = [
    eAppLamStrict
  , eApp1 smallStep
  , eApp2 smallStep
  ]

smallStep :: Term n a -> Maybe (Term n a)
smallStep t =
  asum .
  map ($ t) $
  smallStepsStrict

sEval :: Term n a -> Term n a
sEval t = case smallStep t of
  Nothing -> t
  Just u -> sEval u

