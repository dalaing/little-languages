{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Term.AST (
    Term(..)
  , lam_
  ) where

import Control.Monad (ap)
import Data.Foldable (toList)
import Data.List (elemIndex)

import Bound
import Bound.Name
import Bound.Scope

import Prelude.Extras

import Test.QuickCheck

data Term n a =
    Var a
  | Lam (Scope (Name n ()) (Term n) a)
  | App (Term n a) (Term n a)
  | Let [(n, Scope (Name n Int) (Term n) a)] (Scope (Name n Int) (Term n) a)
  | LetRec [(n, Scope (Name n Int) (Term n) a)] (Scope (Name n Int) (Term n) a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Eq n => Eq1 (Term n) where
  (==#) = (==)

instance Ord n => Ord1 (Term n) where
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
  Let bs b >>= g = Let (fmap (fmap (>>>= g)) bs) (b >>>= g)
  LetRec bs b >>= g = Let (fmap (fmap (>>>= g)) bs) (b >>>= g)

lam_ :: Eq n => n -> Term n n -> Term n n
lam_ x e = Lam (abstract1Name x e)

let_ :: Eq n => [(n, Term n n)] -> Term n n -> Term n n
let_ bs b = Let (fmap (fmap abstr) bs) (abstr b)
  where
    names = map fst . reverse $ bs
    len = length bs
    f x = fmap (\y -> len - 1 - y) (x `elemIndex` names)
    abstr = abstractName f

letrec_ :: Eq n => [(n, Term n n)] -> Term n n -> Term n n
letrec_ bs b = LetRec (fmap (fmap abstr) bs) (abstr b)
  where
    names = map fst bs
    abstr = abstractName (`elemIndex` names)

whnf :: Term n a -> Term n a
whnf (App f x) = case whnf f of
  Lam f' -> whnf (instantiate1Name x f')
  f'     -> App f' x
whnf (Let bs b) = whnf (inst b)
  where
    es = map (inst . snd) bs
    inst = instantiateName (es !!)
whnf (LetRec bs b) = whnf (inst b)
  where
    es = map (inst . snd) bs
    inst = instantiateName (es !!)
whnf x = x

nf :: Term n a -> Term n a
nf (Lam e) = Lam . toScope . nf . fromScope $ e
nf (App f x) = case whnf f of
  Lam f' -> nf (instantiate1Name x f')
  f'     -> App (nf f') (nf x)
nf (Let bs b) = nf (inst b)
  where
    es = map (inst . snd) bs
    inst = instantiateName (es !!)
nf (LetRec bs b) = nf (inst b)
  where
    es = map (inst . snd) bs
    inst = instantiateName (es !!)
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

genLet' :: Eq a
        => Gen a 
        -> Int 
        -> Gen (Term a a)
genLet' genVar s = _

genLetRec' :: Eq a
           => Gen a 
           -> Int 
           -> Gen (Term a a)
genLetRec' genVar s = _

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
shrinkTerm (Let bs b) = 
  fmap (instantiateName (Var ((map fst . reverse $ bs) !!))) (b : map snd bs)
shrinkTerm (LetRec bs b) = [] -- TODO

instance Arbitrary (Term String String) where
  arbitrary = sized (genTerm' genFreshVar)
  shrink = shrinkTerm


