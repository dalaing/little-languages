{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Term.AST (
    Term(..)
  , lam_
  ) where

import Control.Monad (ap)
import Data.Foldable (toList)

import Bound
import Bound.Name
import Bound.Scope

import Prelude.Extras

import Test.QuickCheck

data Term n a =
    Var a
  | Lam (Scope (Name n ()) (Term n) a)
  | App (Term n a) (Term n a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq1 (Term n) where
  (==#) = (==)

instance Ord1 (Term n) where
  compare1 = compare

instance Show n => Show1 (Term n) where 
  showsPrec1 = showsPrec

instance Applicative (Term n) where
  pure = return
  (<*>) = ap

instance Monad (Term n) where
  return = Var

  Var x >>= g   = g x
  Lam e >>= g   = Lam (e >>>= g)
  App f x >>= g = App (f >>= g) (x >>= g)

lam_ :: Eq n => n -> Term n n -> Term n n
lam_ x e = Lam (abstract1Name x e)

whnf :: Term n a -> Term n a
whnf (App f x) = case whnf f of
  Lam f' -> whnf (instantiate1Name x f')
  f'     -> App f' x
whnf x = x

nf :: Term n a -> Term n a
nf (Lam e) = Lam . toScope . nf . fromScope $ e
nf (App f x) = case whnf f of
  Lam f' -> nf (instantiate1Name x f')
  f'     -> App (nf f') (nf x)
nf x = x

-- want arbitrary here so that we can shrink terms

freshVars :: [String]
freshVars = (:) <$> ['a'..'z'] <*> map show ([1..] :: [Int])

genFreshVar :: Gen String
genFreshVar = (freshVars !!) <$> arbitrary

-- TODO Reader for Gen a
genLam' :: Eq a 
        => Gen a 
        -> Int 
        -> Gen (Term a a)
genLam' genVar s = do
  e <- suchThat (genTerm' genVar s) (not . isClosed)
  x <- elements (toList e)
  return $ lam_ x e

genApp' :: Eq a
        => Gen a 
        -> Int 
        -> Gen (Term a a)
genApp' genVar s = 
    App <$> subTerm <*> subTerm
  where
    subTerm = genTerm' genVar s

genTerm' :: Eq a 
         => Gen a 
         -> Int 
         -> Gen (Term a a)
genTerm' genVar 0 = Var <$> genVar
genTerm' genVar s = 
  oneof [
    Var <$> genVar
  , genLam' genVar (s - 1) 
  , genApp' genVar (s `div` 2)
  ]

genRedex' :: Eq a
          => Gen a
          -> Int
          -> Gen (Term a a)
genRedex' genVar s =
  App <$> genLam' genVar s <*> genTermWithRedex' genVar s

-- TODO we want the redex at the top, so replace App with Redex
--   we can use genTerm in genRedex, because we don't care so much after
--   that
-- TODO we may also care that all apps have something that reduces to a lam
-- as the first argument, we should be able to generate that
-- then the normal form would result in a var or a lam
genTermWithRedex' :: Eq a
                  => Gen a
                  -> Int
                  -> Gen (Term a a)
genTermWithRedex' genVar 0 = Var <$> genVar
genTermWithRedex' genVar s = 
  oneof [
    Var <$> genVar
  , genLam' genVar (s - 1) 
  , genApp' genVar (s `div` 2)
  , genRedex' genVar (s `div` 2)
  ]

genLeftNestedRedex' :: Eq a
          => Gen a
          -> Int
          -> Gen (Term a a)
genLeftNestedRedex' genVar s =
  App <$> oneof [genLam' genVar s, genLeftNestedRedex' genVar s] <*> genTermWithLeftNestedRedex' genVar s

genTermWithLeftNestedRedex' :: Eq a
                  => Gen a
                  -> Int
                  -> Gen (Term a a)
genTermWithLeftNestedRedex' genVar 0 = Var <$> genVar
genTermWithLeftNestedRedex' genVar s = 
  oneof [
    Var <$> genVar
  , genLam' genVar (s - 1) 
  , genApp' genVar (s `div` 2)
  , genLeftNestedRedex' genVar (s `div` 2)
  ]

freshVarForLam :: Scope (Name String ()) (Term String) String -> String
freshVarForLam s = head . dropWhile (`elem` freeVars) $ freshVars
  where
    freeVars = toList s

shrinkTerm :: Term String String -> [Term String String]
shrinkTerm (Var x) = 
  [Var x]
shrinkTerm (Lam e) = 
  [instantiate1Name (Var . name . head . bindings $ e) e]
shrinkTerm (App f x) = 
  [f, x] ++ [App f' x' | (f', x') <- shrink (f, x)]

instance Arbitrary (Term String String) where
  arbitrary = sized (genTerm' genFreshVar)
  shrink = shrinkTerm


