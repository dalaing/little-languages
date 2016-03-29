module Term.Eval.BigStep where

import Control.Lens (preview, review)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Term
import Term.Eval.Value (bv)

eZero :: Term l
      -> Maybe (Term l)
eZero t = do
  _ <- preview _TmZero t
  return TmZero

eSucc :: (Term l -> Maybe (Term l))
      -> Term l
      -> Maybe (Term l)
eSucc step t = do
  u <- preview _TmSucc t
  v <- step u
  return $ review _TmSucc v

ePredZero :: (Term l -> Maybe (Term l))
          -> Term l
          -> Maybe (Term l)
ePredZero step t = do
  u <- preview _TmPred t
  v <- step u
  preview _TmZero v
  return $ review _TmZero ()

ePredSucc :: (Term l -> Maybe (Term l))
          -> Term l
          -> Maybe (Term l)
ePredSucc step t = do
  u <- preview _TmPred t
  v <- step u
  preview _TmSucc v

eIfTrue :: (Term l -> Maybe (Term l))
        -> Term l
        -> Maybe (Term l)
eIfTrue step t = do
    (t1, t2, _) <- preview _TmIf t
    u1 <- step t1
    preview _TmTrue u1
    step t2

eIfFalse :: (Term l -> Maybe (Term l))
         -> Term l
         -> Maybe (Term l)
eIfFalse step t = do
    (t1, _, t3) <- preview _TmIf t
    u1 <- step t1
    preview _TmFalse u1
    step t3

eIsZeroZero :: (Term l -> Maybe (Term l))
            -> Term l
            -> Maybe (Term l)
eIsZeroZero step t = do
  u <- preview _TmIsZero t
  v <- step u
  _ <- preview _TmZero v
  return $ review _TmTrue ()

eIsZeroSucc :: (Term l -> Maybe (Term l))
            -> Term l
            -> Maybe (Term l)
eIsZeroSucc step t = do
  u <- preview _TmIsZero t
  v <- step u
  _ <- preview _TmSucc v
  return $ review _TmFalse ()

eLoc :: (Term l -> Maybe (Term l))
     -> Term l
     -> Maybe (Term l)
eLoc step t = do
  (l, u) <- preview _TmLoc t
  v <- step u
  return $ review _TmLoc (l, v)

bigSteps :: [Term l -> Maybe (Term l)]
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
  , eLoc bigStep
  ]

bigStep :: Term l
        -> Maybe (Term l)
bigStep t =
  asum .
  map ($ t) $
  bigSteps

bEval :: Term l
      -> Term l
bEval t =
  fromMaybe t .
  bigStep $
  t

