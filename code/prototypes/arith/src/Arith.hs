{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Arith where

import Test.QuickCheck as QC

import Control.Lens

import Rules

data Term = 
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmNat Term
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Ord, Show)

makePrisms ''Term

genBool :: Gen Term
genBool = QC.elements [TmTrue, TmFalse]

genNat :: Gen Term
genNat = sized genNat'
  where
    genNat' 0 = return TmZero
    genNat' s = oneof [return TmZero, TmSucc <$> genNat' (s - 1)]

genValue :: Gen Term
genValue = oneof [genBool, genNat]


liftL3 :: Prism' s1 a1 -> Prism' s2 a2 -> Prism' s3 a3 -> Prism' (s1, s2, s3) (a1, a2, a3)
liftL3 p1 p2 p3 = prism 
  (\(b1, b2, b3) -> (review p1 b1, review p2 b2, review p3 b3))
  (\s@(s1, s2, s3) -> let
      x = (,,) <$> preview p1 s1 <*> preview p2 s2 <*> preview p3 s3
    in
      case x of
          Nothing -> Left s
          Just y -> Right y
   )

isNumVal :: Term -> Bool
isNumVal TmZero = True
isNumVal (TmSucc s) = isNumVal s
isNumVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = False
isVal x 
    | isNumVal x = True
    | otherwise  = False

data Type =
    TyBool
  | TyInt
  | TyArrow Type Type

eval :: Term -> Maybe Term
eval (TmIf TmTrue t _) = eval t
eval (TmIf TmFalse _ t) = eval t
eval (TmIf t1 t2 t3) = TmIf <$> eval t1 <*> pure t2 <*> pure t3
eval (TmSucc t) = TmSucc <$> eval t
eval (TmPred TmZero) = Just TmZero
eval (TmPred (TmSucc t)) = eval t
eval (TmPred t) = TmPred <$> eval t
eval (TmIsZero TmZero) = Just TmTrue
eval (TmIsZero (TmSucc _)) = Just TmFalse
eval (TmIsZero t) = TmIsZero <$> eval t
eval x
    | isVal x = Just x
    | otherwise = Nothing

eIfTrue :: Term -> Maybe Term
eIfTrue (TmIf TmTrue t _) = Just t
eIfTrue _ = Nothing
-- eIfTrue = preview $ _TmIf . liftL3 _TmTrue id id . _2

eIfFalse :: Term -> Maybe Term
eIfFalse (TmIf TmFalse _ t) = Just t
eIfFalse _ = Nothing
-- eIfFalse = preview $ _TmIf . liftL3 _TmFalse id id . _3

eIf :: (Term -> Term) -> Term -> Maybe Term
eIf step (TmIf t1 t2 t3) = Just (TmIf (step t1) t2 t3)
eIf _ _ = Nothing

-- TmIf TmTrue TmZero TmZero & _TmIf . _1 %~ const TmFalse
-- > TmIf TmFalse TmZero tmZero
-- TmZero & _TmIf . _1 %~ const TmFalse
-- > tmZero
-- need a version which gives nothing when it fails, gives just when it
-- works (and also applies the update)

eSucc :: (Term -> Term) -> Term -> Maybe Term
eSucc step (TmSucc t) = Just $ TmSucc (step t)
eSucc _ _ = Nothing

ePredZero :: Term -> Maybe Term
ePredZero (TmPred TmZero) = Just TmZero
ePredZero _ = Nothing

ePredSucc :: Term -> Maybe Term
ePredSucc (TmPred (TmSucc t)) = Just t
ePredSucc _ = Nothing

ePred :: (Term -> Term) -> Term -> Maybe Term
ePred step (TmPred t) = Just $ TmPred (step t)
ePred _ _ = Nothing

eIsZeroZero :: Term -> Maybe Term
eIsZeroZero (TmIsZero TmZero) = Just TmTrue
eIsZeroZero _ = Nothing

eIsZeroSucc :: Term -> Maybe Term
eIsZeroSucc (TmIsZero (TmSucc _)) = Just TmFalse
eIsZeroSucc _ = Nothing

eIsZero :: (Term -> Term) -> Term -> Maybe Term
eIsZero step (TmIsZero t) = Just $ TmIsZero (step t)
eIsZero _ _ = Nothing

arithRules :: Rules Term Term
arithRules = Rules $ map Axiom axioms ++ map Step steps
  where
    axioms = [
        eIfTrue
      , eIfFalse
      , ePredZero
      , ePredSucc
      , eIsZeroZero
      , eIsZeroSucc
      ]
    steps = [
        eIf
      , eSucc
      , ePred
      , eIsZero
      ]

arithEval :: Term -> Term
arithEval = makeEval arithRules




