{-# LANGUAGE RankNTypes #-}
module Term.Gen where

import Control.Lens (preview, review)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Bound

import Test.QuickCheck (Gen, Arbitrary(..), oneof, sized, elements, listOf)

import Term

genTmVar :: Gen a -> Gen (Term a a)
genTmVar g = TmVar <$> g

genTmApp :: Gen (Term a a) -> Gen (Term a a) -> Gen (Term a a)
genTmApp g1 g2 = TmApp <$> g1 <*> g2

-- we probably want genContaining here, so that g2 contains TmVar g1
genTmLam :: Eq a => Gen a -> Gen (Term a a) -> Gen (Term a a)
genTmLam g1 g2 = curry (review _lam) <$> g1 <*> g2

-- we possibly want a well-typed / ill-typed divide here
-- so that App Var gets stuck for the ill typed version
genRedex :: Eq a => Gen a -> Gen (Term a a) -> Gen (Term a a)
genRedex g1 g2 = sized (genRedex' g1 g2)

genRedex' :: Eq a => Gen a -> Gen (Term a a) -> Int -> Gen (Term a a)
genRedex' g1 g2 s =
  oneof $ (if s == 0 then [] else [genTmApp (genTmLam g1 (genRedex' g1 g2 (s `div` 2))) g2]) ++ [genTmLam g1 g2]

genTerm :: Eq a => Gen a -> Gen (Term a a)
genTerm g = sized (genTerm' g)

genTerm' :: Eq a => Gen a -> Int -> Gen (Term a a)
genTerm' g s =
  let
    zeroSize = [genTmVar g]
    child = genTerm' g (s `div` 2)
    nonZeroSize = [genTmApp (genRedex g child) child, genTmLam g child]
  in
    oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

genTermContains :: Eq a => Gen a -> Term a a -> Gen (Term a a)
genTermContains g t = sized (genTermContains' g t)

genTermContains' :: Eq a => Gen a -> Term a a -> Int -> Gen (Term a a)
genTermContains' g t s =
  let
    zeroSize = [pure t]
    s' = s `div` 2
    childContains = genTermContains' g t s'
    child = genTerm' g s'
    nonZeroSize = [
        genTmApp childContains child
      , genTmApp child childContains
      , genTmLam g childContains
      ]
  in
    oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

shrTmVar :: Term n a -> Maybe [Term n a]
shrTmVar = fmap (const []) . preview _TmVar

shrTmApp :: (Term n a -> [Term n a])
         -> (Term n a -> [Term n a])
         -> Term n a
         -> Maybe [Term n a]
shrTmApp shr1 shr2 = fmap shrTmApp' . preview _TmApp
  where
    shrTmApp' (x, y) =
      fmap (\x' -> TmApp x' y) (shr1 x) ++
      fmap (\y' -> TmApp x y') (shr2 y)

shrTmLam :: (forall x. Term n x -> [Term n x])
         -> Term n a
         -> Maybe [Term n a]
shrTmLam shr = fmap shrTmLam' . preview _TmLam
  where
    shrTmLam' (n, e) = fmap (TmLam n . toScope) . shr . fromScope $ e

shrinkTerm :: Term n a -> [Term n a]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    shrTmVar
  , shrTmApp shrinkTerm shrinkTerm
  , shrTmLam shrinkTerm
  ]

genVar :: Gen String
genVar = (:) <$> elements lower <*> listOf (elements alphaNum)
  where
    lower = ['a'..'z']
    upper = ['A'..'Z']
    num = ['0'..'9']
    alphaNum = lower ++ upper ++ num

-- TODO use pretty printer for the show here
newtype AnyTerm = AnyTerm { getAnyTerm :: Term String String }
                deriving (Eq, Ord, Show)

instance Arbitrary AnyTerm where
  arbitrary = AnyTerm <$> genTerm genVar
  shrink = fmap AnyTerm . shrinkTerm . getAnyTerm
