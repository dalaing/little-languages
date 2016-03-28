module Term.Eval.BigStep where

import Control.Lens (preview)
import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Term
import Term.Eval.Value (bv)

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

eIsZeroZero :: (Term -> Maybe Term)
            -> Term
            -> Maybe Term
eIsZeroZero step t = do
  u <- preview _TmIsZero t
  v <- step u
  _ <- preview _TmZero v
  return TmTrue

eIsZeroSucc :: (Term -> Maybe Term)
            -> Term
            -> Maybe Term
eIsZeroSucc step t = do
  u <- preview _TmIsZero t
  v <- step u
  _ <- preview _TmSucc v
  return TmFalse

bigSteps :: [Term -> Maybe Term]
bigSteps =
  [ bv
  , eZero
  , eSucc bigStep
  , ePredZero bigStep
  , ePredSucc bigStep
  , eIfTrue bigStep
  , eIfFalse bigStep
  , eIsZeroZero bigStep
  , eIsZeroSucc bigStep
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

