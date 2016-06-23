{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Term.AST (
    Term(..)
  , lam_
  , freshVarForLam
  ) where

import Control.Monad (ap)
import Data.Foldable (toList)

import Bound
import Prelude.Extras

import Test.QuickCheck

data Term a =
    Var a
  | Lam (Scope () Term a)
  | App (Term a) (Term a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq1 Term where
  (==#) = (==)

instance Ord1 Term where
  compare1 = compare

instance Show1 Term where 
  showsPrec1 = showsPrec

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = Var

  Var x >>= g   = g x
  Lam e >>= g   = Lam (e >>>= g)
  App f x >>= g = App (f >>= g) (x >>= g)

lam_ :: Eq a => a -> Term a -> Term a
lam_ x e = Lam (abstract1 x e)

whnf :: Term a -> Term a
whnf (App f x) = case whnf f of
  Lam f' -> whnf (instantiate1 x f')
  f'     -> App f' x
whnf x = x

nf :: Term a -> Term a
nf (Lam e) = Lam . toScope . nf . fromScope $ e
nf (App f x) = case whnf f of
  Lam f' -> nf (instantiate1 x f')
  f'     -> App (nf f') (nf x)
nf x = x

-- want arbitrary here so that we can shrink terms

freshVars :: [String]
freshVars = (:) <$> ['a'..'z'] <*> map show ([1..] :: [Int])

genFreshVar :: Gen String
genFreshVar = ((freshVars !!) . getNonNegative) <$> arbitrary

-- TODO Reader for Gen a
-- TODO we might want to distinguish between 
--  - lambdas where the binder is used
--  - lambdas where the binder is not used
-- TODO we probably also want an explicit gen for lambdas / terms with free
-- variables
genLam' :: Eq a
               => Gen a 
               -> Int 
               -> Gen (Term a)
genLam' genVar s = do
  e <- suchThat (genTerm' genVar s) (not . isClosed)
  x <- elements (toList e)
  return $ lam_ x e

genApp' :: Eq a
               => Gen a 
               -> Int 
               -> Gen (Term a)
genApp' genVar s = 
    App <$> subTerm <*> subTerm
  where
    subTerm = genTerm' genVar s

genTerm' :: Eq a 
                => Gen a 
                -> Int 
                -> Gen (Term a)
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
          -> Gen (Term a)
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
                  -> Gen (Term a)
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
          -> Gen (Term a)
genLeftNestedRedex' genVar s =
  App <$> oneof [genLam' genVar s, genLeftNestedRedex' genVar s] <*> genTermWithLeftNestedRedex' genVar s

genTermWithLeftNestedRedex' :: Eq a
                  => Gen a
                  -> Int
                  -> Gen (Term a)
genTermWithLeftNestedRedex' genVar 0 = Var <$> genVar
genTermWithLeftNestedRedex' genVar s = 
  oneof [
    Var <$> genVar
  , genLam' genVar (s - 1) 
  , genApp' genVar (s `div` 2)
  , genLeftNestedRedex' genVar (s `div` 2)
  ]

freshVarForLam :: Scope () Term String -> String
freshVarForLam s = head . dropWhile (`elem` freeVars) $ freshVars
  where
    freeVars = toList s

shrinkTerm :: Term String -> [Term String]
shrinkTerm (Var x) = 
  [Var x]
shrinkTerm (Lam e) = 
  [instantiate1 (Var . freshVarForLam $ e) e]
shrinkTerm (App f x) = 
  [f, x] ++ [App f' x' | (f', x') <- shrink (f, x)]

instance Arbitrary (Term String) where
  arbitrary = sized (genTerm' genFreshVar)
  shrink = shrinkTerm


