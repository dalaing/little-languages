module Term.Eval.BigStep where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Term
import Term.Eval.Value (value)

eZero :: Term
      -> Maybe Term
eZero t = do
  _ <- preview _TmZero t
  return TmZero

eSucc :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eSucc step t = do
  u <- preview _TmSucc t
  v <- step u
  return $ TmSucc v

ePredZero :: (Term -> Maybe Term)
          -> Term
          -> Maybe Term
ePredZero step t = do
  u <- preview _TmPred t
  v <- step u
  preview _TmZero v
  return TmZero

ePredSucc :: (Term -> Maybe Term)
          -> Term
          -> Maybe Term
ePredSucc step t = do
  u <- preview _TmPred t
  v <- step u
  preview _TmSucc v

bigStepRules :: [Term -> Maybe Term]
bigStepRules =
  [ eZero
  , eSucc bigStep
  , ePredZero bigStep
  , ePredSucc bigStep
  ]

bigStep :: Term
        -> Maybe Term
bigStep t =
  asum .
  map ($ t) $
  bigStepRules

bEval :: Term
      -> Term
bEval t =
  fromMaybe t .
  bigStep $
  t

