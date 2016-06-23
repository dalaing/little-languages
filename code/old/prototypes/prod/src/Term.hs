{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Term (
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

import Decl
import Type

-- TODO a pattern for multiple bindings, to help deal with primops
-- although primops could be represented as Scope Int Exp a, where the
-- arguments are indexed backwards (so the indices stay valid over partial
-- application)
data Pat l n d =
    VarP n
  | WildP
  | AsP n (Pat l n d)
  | ConP d [Pat l n d]
  | LocP l (Pat l n d)
  deriving (Eq, Ord, Show)

data P l n d = P { pat :: [n] -> Pat l n d, patBindings :: [n] }

varp :: n -> P l n d
varp a = P (const (VarP a)) [a]

wildp :: P l n d
wildp = P (const WildP) []

asp :: n -> P l n d -> P l n d
asp a (P p as) = P (\bs -> AsP a (p (a:bs))) (a:as)

conp :: d -> [P l n d] -> P l n d
conp d ps = P (ConP d . go ps) (ps >>= patBindings)
  where
    go (P p as:ps) bs = p bs : go ps (bs ++ as)
    go [] _ = []

data Term l n d a =
    Var a
  -- | Lam (Type l n d) (Scope (Name n ()) (Term l n d) a)
  | Lam (Type l n d) (Pat l n d) (Scope (Name n Int) (Term l n d) a)
  | App (Term l n d a) (Term l n d a)
  | Loc l (Term l n d a)
  | TmInt Int
  | TmBool Bool
  | Add (Term l n d a) (Term l n d a)
  | Equ (Term l n d a) (Term l n d a)
  | And (Term l n d a) (Term l n d a)
  | Con (Scope (Name n Int) (Decl l n (Type l n d)) d) [Term l n d a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq l, Eq n, Eq d) => Eq1 (Term l n d) where
  (==#) = (==)

instance (Ord l, Ord n, Ord d) => Ord1 (Term l n d) where
  compare1 = compare

instance (Show l, Show n, Show d) => Show1 (Term l n d) where 
  showsPrec1 = showsPrec

instance Applicative (Term l n d) where
  pure = return
  (<*>) = ap

instance Monad (Term l n d) where
  return = Var

  Var x >>= g   = g x
  Lam t p e >>= g = Lam t p (e >>>= g)
  App f x >>= g = App (f >>= g) (x >>= g)
  Loc l x >>= g = Loc l (x >>= g)

  TmInt i >>= _ = TmInt i
  TmBool b >>= _ = TmBool b

  Add x y >>= g = Add (x >>= g) (y >>= g)
  Equ x y >>= g = Equ (x >>= g) (y >>= g)
  And x y >>= g = And (x >>= g) (y >>= g)

  Con d xs >>= g = Con d (map (>>= g) xs)

lam_ :: Eq n => Type l n d -> P l n d -> Term l n d n -> Term l n d n
lam_ t (P p as) e = Lam t (p []) (abstractName (`elemIndex` as) e)

-- | Test
-- >>> 1 +2
-- 4
patternTerms :: Pat l n d -> Term l n d a -> [Term l n d a]
patternTerms (VarP _) e = [e]
patternTerms WildP _ = []
patternTerms (AsP _ p) e = e : patternTerms p e
patternTerms (ConP _ ps) (Con _ es) = concat $ zipWith patternTerms ps es
patternTerms (ConP _ ps) _ = []
patternTerms (LocP _ p) e = patternTerms p e

inst :: Pat l n d -> Term l n d a -> Scope (Name n Int) (Term l n d) a -> Term l n d a
inst p e = instantiateName (patternTerms p e !!)

whnf :: Term l n d a -> Term l n d a
whnf (App f x) = case whnf f of
  Lam _ p f' -> whnf (inst p x f')
  f'     -> App f' x
whnf x = x

nf :: Term l n d a -> Term l n d a
nf (Lam t p e) = Lam t p . toScope . nf . fromScope $ e
nf (App f x) = case whnf f of
  Lam _ p f' -> nf (inst p x f')
  f'     -> App (nf f') (nf x)
nf (Add x y) = case (nf x, nf y) of
  (TmInt x', TmInt y') -> TmInt (x' + y')
  (x', y') -> Add x' y'
nf (Equ x y) = case (nf x, nf y) of
  (TmInt x', TmInt y') -> TmBool (x' == y')
  (x', y') -> Equ x' y'
nf (And x y) = case (nf x, nf y) of
  (TmBool x', TmBool y') -> TmBool (x' && y')
  (x', y') -> And x' y'
nf (Con d xs) = Con d (map nf xs)
nf x = x

-- want arbitrary here so that we can shrink terms

freshVars :: [String]
freshVars = (:) <$> ['a'..'z'] <*> map show ([1..] :: [Int])

genFreshVar :: Gen String
genFreshVar = ((freshVars !!) . getNonNegative) <$> arbitrary

-- TODO Reader for Gen a
genLam' :: Eq a
        => Gen a
        -> Int
        -> Gen (Term l a d a)
genLam' genVar s = do
  e <- suchThat (genTerm' genVar s) (not . isClosed)
  x <- elements (toList e)
  return $ lam_ undefined (varp x) e

genApp' :: Eq a
        => Gen a 
        -> Int 
        -> Gen (Term l a d a)
genApp' genVar s = 
    App <$> subTerm <*> subTerm
  where
    subTerm = genTerm' genVar s

genTerm' :: Eq a 
         => Gen a 
         -> Int 
         -> Gen (Term l a d a)
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
          -> Gen (Term l a d a)
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
                  -> Gen (Term l a d a)
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
          -> Gen (Term l a d a)
genLeftNestedRedex' genVar s =
  App <$> oneof [genLam' genVar s, genLeftNestedRedex' genVar s] <*> genTermWithLeftNestedRedex' genVar s

genTermWithLeftNestedRedex' :: Eq a
                  => Gen a
                  -> Int
                  -> Gen (Term l a d a)
genTermWithLeftNestedRedex' genVar 0 = Var <$> genVar
genTermWithLeftNestedRedex' genVar s = 
  oneof [
    Var <$> genVar
  , genLam' genVar (s - 1) 
  , genApp' genVar (s `div` 2)
  , genLeftNestedRedex' genVar (s `div` 2)
  ]

freshVarForLam :: Scope (Name String ()) (Term l String d) String -> String
freshVarForLam s = head . dropWhile (`elem` freeVars) $ freshVars
  where
    freeVars = toList s

shrinkTerm :: Term l String d String -> [Term l String d String]
shrinkTerm (Var x) = 
  [Var x]
shrinkTerm (Lam _ _ e) = 
  [instantiate1Name (Var . name . head . bindings $ e) e]
shrinkTerm (App f x) = 
  [f, x] ++ [App f' x' | (f', x') <- shrink (f, x)]

instance Arbitrary (Term l String d String) where
  arbitrary = sized (genTerm' genFreshVar)
  shrink = shrinkTerm


