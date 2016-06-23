{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Term (
    Term(..)
  , lam_
  , whnf
  , nf
  ) where

import Data.List (isInfixOf)

import Control.Monad (ap)

import Prelude.Extras
import Bound
import Bound.Name

data Term n a =
    Var a
  | Lam (Scope (Name n ()) (Term n) a)
  | App (Term n a) (Term n a)
  | LitI Int
  | EqI (Term n a) (Term n a)
  | Add (Term n a) (Term n a)
  | Mul (Term n a) (Term n a)
  | Lt (Term n a) (Term n a)
  | Lte (Term n a) (Term n a)
  | Gt (Term n a) (Term n a)
  | Gte (Term n a) (Term n a)
  | LitB Bool
  | EqB (Term n a) (Term n a)
  | And (Term n a) (Term n a)
  | Or (Term n a) (Term n a)
  | Not (Term n a)
  | LitS String
  | EqS (Term n a) (Term n a)
  | In (Term n a) (Term n a)
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

  Var x >>= g = g x
  Lam e >>= g = Lam (e >>>= g)
  App f x >>= g = App (f >>= g) (x >>= g)

  LitI i >>= _ = LitI i
  EqI x y >>= g = EqI (x >>= g) (y >>= g)
  Lt x y >>= g = Lt (x >>= g) (y >>= g)
  Lte x y >>= g = Lte (x >>= g) (y >>= g)
  Gt x y >>= g = Gt (x >>= g) (y >>= g)
  Gte x y >>= g = Gte (x >>= g) (y >>= g)
  Add x y >>= g = Add (x >>= g) (y >>= g)
  Mul x y >>= g = Mul (x >>= g) (y >>= g)

  LitB b >>= _ = LitB b
  EqB x y >>= g = EqB (x >>= g) (y >>= g)
  And x y >>= g = And (x >>= g) (y >>= g)
  Or x y >>= g = Or (x >>= g) (y >>= g)
  Not x >>= g = Not (x >>= g)

  LitS s >>= _ = LitS s
  EqS x y >>= g = EqS (x >>= g) (y >>= g)
  In x y >>= g = In (x >>= g) (y >>= g)

lam_ :: Eq n => n -> Term n n -> Term n n
lam_ x e = Lam (abstract1Name x e)

whnf :: Term n a -> Term n a
whnf (App f x) = case whnf f of
  Lam f' -> whnf (instantiate1Name x f')
  f'     -> App f' x
whnf (EqI x y) = case (whnf x, y) of
  (LitI x', LitI y') -> LitB (x' == y')
  (x', y')     -> EqI x' y'
whnf (Lt x y) = case (whnf x, y) of
  (LitI x', LitI y') -> LitB (x' < y')
  (x', y')     -> EqI x' y'
whnf (Lte x y) = case (whnf x, y) of
  (LitI x', LitI y') -> LitB (x' <= y')
  (x', y')     -> EqI x' y'
whnf (Gt x y) = case (whnf x, y) of
  (LitI x', LitI y') -> LitB (x' > y')
  (x', y')     -> EqI x' y'
whnf (Gte x y) = case (whnf x, y) of
  (LitI x', LitI y') -> LitB (x' >= y')
  (x', y')     -> EqI x' y'
whnf (Add x y) = case (whnf x, y) of
  (LitI x', LitI y') -> LitI (x' + y')
  (x', y')     -> EqI x' y'
whnf (Mul x y) = case (whnf x, y) of
  (LitI x', LitI y') -> LitI (x' * y')
  (x', y')     -> EqI x' y'
whnf (EqB x y) = case (whnf x, y) of
  (LitB x', LitB y') -> LitB (x' == y')
  (x', y')     -> EqI x' y'
whnf (And x y) = case whnf x of
  LitB False -> LitB False
  LitB True -> whnf y
  x' -> And x' y
whnf (Or x y) = case whnf x of
  LitB True -> LitB True
  LitB False -> whnf y
  x' -> Or x' y
whnf (Not x) = case whnf x of
  LitB b -> LitB (not b)
  x' -> Not x'
whnf (EqS x y) = case (whnf x, y) of
  (LitS x', LitS y') -> LitB (x' == y')
  (x', y')     -> EqI x' y'
whnf (In x y) = case (whnf x, y) of
  (LitS x', LitS y') -> LitB (x' `isInfixOf` y')
  (x', y')     -> EqI x' y'
whnf x = x

nf :: Term n a -> Term n a
nf (App f x) = case whnf f of
  Lam f' -> nf (instantiate1Name x f')
  f' -> App (nf f') (nf x)
nf (EqI x y) = case (nf x, nf y) of
  (LitI x', LitI y') -> LitB (x' == y')
  (x', y')     -> EqI x' y'
nf (Lt x y) = case (nf x, nf y) of
  (LitI x', LitI y') -> LitB (x' < y')
  (x', y')     -> EqI x' y'
nf (Lte x y) = case (nf x, nf y) of
  (LitI x', LitI y') -> LitB (x' <= y')
  (x', y')     -> EqI x' y'
nf (Gt x y) = case (nf x, nf y) of
  (LitI x', LitI y') -> LitB (x' > y')
  (x', y')     -> EqI x' y'
nf (Gte x y) = case (nf x, nf y) of
  (LitI x', LitI y') -> LitB (x' >= y')
  (x', y')     -> EqI x' y'
nf (Add x y) = case (nf x, nf y) of
  (LitI x', LitI y') -> LitI (x' + y')
  (x', y')     -> EqI x' y'
nf (Mul x y) = case (nf x, nf y) of
  (LitI x', LitI y') -> LitI (x' * y')
  (x', y')     -> EqI x' y'
nf (EqB x y) = case (nf x, nf y) of
  (LitB x', LitB y') -> LitB (x' == y')
  (x', y')     -> EqI x' y'
nf (And x y) = case nf x of
  LitB False -> LitB False
  LitB True -> nf y
  x' -> And x' (nf y)
nf (Or x y) = case nf x of
  LitB True -> LitB True
  LitB False -> nf y
  x' -> Or x' (nf y)
nf (Not x) = case nf x of
  LitB b -> LitB (not b)
  x' -> Not x'
nf (EqS x y) = case (nf x, nf y) of
  (LitS x', LitS y') -> LitB (x' == y')
  (x', y')     -> EqI x' y'
nf (In x y) = case (nf x, nf y) of
  (LitS x', LitS y') -> LitB (x' `isInfixOf` y')
  (x', y')     -> EqI x' y'
nf x = x

