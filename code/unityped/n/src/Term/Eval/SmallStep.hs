module Term.Eval.SmallStep where

import Control.Lens (preview)
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
  _ <- value u
  preview _TmSucc u

ePred :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
ePred step t = do
  u <- preview _TmPred t
  v <- step u
  return $ TmPred v

eSucc :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eSucc step t = do
  u <- preview _TmSucc t
  v <- step u
  return $ TmSucc v

smallSteps :: [Term -> Maybe Term]
smallSteps = [ePredZero, ePredSucc, ePred smallStep, eSucc smallStep]

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

