{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module STLC.Classy where

import           Control.Applicative
import           Control.Monad       (ap)
import           Data.Bifunctor
import           Data.Foldable       (toList)
import           Data.List           (elemIndex)

import           Bound
import           Bound.Name
import           Bound.Scope

import           Prelude.Extras

import           Test.QuickCheck

import           Control.Lens
import           Control.Lens.Extras

-- import           STLC.Type

-- TODO classy prisms here
-- we want to be able to make some things available when ints / bools
-- / both are available

class AsTyInt t where
  _TyInt :: Prism' t ()

class AsTyBool t where
  _TyBool :: Prism' t ()

class AsTyArr t where
  _TyArr :: Prism' t (t, t)

data Type = TyInt | TyBool | TyArr Type Type
          deriving (Eq, Ord, Show)

instance AsTyInt Type where
  _TyInt = prism (const TyInt) $ \t -> case t of
    TyInt -> Right ()
    _ -> Left t

instance AsTyBool Type where
  _TyBool = prism (const TyBool) $ \t -> case t of
    TyBool -> Right ()
    _ -> Left t

instance AsTyArr Type where
  _TyArr = prism (uncurry TyArr) $ \t -> case t of
    TyArr x y -> Right (x, y)
    _ -> Left t

class IsValue f where
  isValue :: f -> Bool

data LcF n t f a =
    TmVar a
  | TmLam t (Scope (Name n ()) f a)
  | TmApp (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''LcF

instance IsValue (LcF n t f a) where
  isValue = is _TmLam

type AsLc n t f a = (AsTyArr t, AsLcF (f n t a) n t (f n t) a)

data IntF t f a =
    TmInt Int
  | TmAdd (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bound (IntF t) where
  TmInt i >>>= _ = TmInt i
  TmAdd x y >>>= g = TmAdd (x >>= g) (y >>= g)

makeClassyPrisms ''IntF

instance IsValue (IntF t f a) where
  isValue = is _TmInt

type AsInt t g a = (AsTyInt t, AsIntF (g a) t g a)

data BoolF t f a =
    TmBool Bool
  | TmAnd (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bound (BoolF t) where
  TmBool b >>>= _ = TmBool b
  TmAnd x y >>>= g = TmAnd (x >>= g) (y >>= g)

makeClassyPrisms ''BoolF

instance IsValue (BoolF t f a) where
  isValue = is _TmBool

type AsBool t g a = (AsTyBool t, AsBoolF (g a) t g a)

data EqF t f a =
    TmEq (f a) (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bound (EqF t) where
  TmEq x y >>>= g = TmEq (x >>= g) (y >>= g)

makeClassyPrisms ''EqF

instance IsValue (EqF t f a) where
  isValue = const False

type AsEq t g a = (AsTyInt t, AsTyBool t, AsEqF (g a) t g a)

data TermF l n t f a =
    TmLoc l (f a)
  | LcPart (LcF n t f a)
  | IntPart (IntF t f a)
  | BoolPart (BoolF t f a)
  | EqPart (EqF t f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TermF

instance AsLcF (TermF l n t f a) n t f a where
  _LcF = _LcPart

instance AsIntF (TermF l n t f a) t f a where
  _IntF = _IntPart

instance AsBoolF (TermF l n t f a) t f a where
  _BoolF = _BoolPart

instance AsEqF (TermF l n t f a) t f a where
  _EqF = _EqPart

instance IsValue (f a) => IsValue (TermF l n t f a) where
  isValue (TmLoc _ t) = isValue t
  isValue (LcPart t) = isValue t
  isValue (IntPart t) = isValue t
  isValue (BoolPart t) = isValue t
  isValue (EqPart t) = isValue t

-- TODO constraint alias here for the full set of parts that appear in TermF

newtype Term l n t a = Term { unTerm :: TermF l n t (Term l n t) a}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq l, Eq n, Eq t) => Eq1 (Term l n t) where
  (==#) = (==)

instance (Ord l, Ord n, Ord t) => Ord1 (Term l n t) where
  compare1 = compare

instance (Show l, Show n, Show t) => Show1 (Term l n t) where
  showsPrec1 = showsPrec

instance Applicative (Term l n t) where
  pure = return
  (<*>) = ap

instance Monad (Term l n t) where
  return = review _TmVar

  Term (LcPart (TmVar x)) >>= g   = g x
  Term tm >>= g = Term $ case tm of
    TmLoc l x -> TmLoc l (x >>= g)

    LcPart (TmLam t e) -> LcPart $ TmLam t (e >>>= g)
    LcPart (TmApp f x) -> LcPart $ TmApp (f >>= g) (x >>= g)

    IntPart i -> IntPart (i >>>= g)
    BoolPart b -> BoolPart (b >>>= g)
    EqPart e -> EqPart (e >>>= g)

instance IsValue (Term l n t a) where
  isValue (Term t) = isValue t

makeWrapped ''Term

instance AsTermF (Term l n t a) l n t (Term l n t) a where
  _TermF = _Wrapped

instance AsLcF (Term l n t a) n t (Term l n t) a where
  _LcF = _TermF . _LcPart

instance AsIntF (Term l n t a) t (Term l n t) a where
  _IntF = _TermF . _IntPart

instance AsBoolF (Term l n t a) t (Term l n t) a where
  _BoolF = _TermF . _BoolPart

instance AsEqF (Term l n t a) t (Term l n t) a where
  _EqF = _TermF . _EqPart

liftL2 :: Prism' s1 a1 -> Prism' s2 a2 -> Prism' (s1, s2) (a1, a2)
liftL2 p1 p2 = prism
  (\(b1, b2) -> (review p1 b1, review p2 b2))
  (\s@(s1, s2) -> let
      x = (,) <$> preview p1 s1 <*> preview p2 s2
    in
      case x of
          Nothing -> Left s
          Just y -> Right y
   )

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

-- lam is a value
-- need types / prisms for values here as well
v :: AsLc n t g a => a -> g n t a
v = review _TmVar

lam_ :: (AsLc n t g n, Monad (g n t), Eq n) => t -> n -> g n t n -> g n t n
lam_ t n e = review _TmLam (t, abstract1Name n e)

infixr 0 !
(!) :: (AsLc n t g n, Monad (g n t), Eq n) => t -> n -> g n t n -> g n t n
(!) = lam_

infixl 9 .@.
(.@.) :: AsLc n t g a => g n t a -> g n t a -> g n t a
(.@.) x y = review _TmApp (x, y)

int :: AsInt t g a => Int -> g a
int = review _TmInt

infixl 6 .+.
(.+.) :: AsInt t g a => g a -> g a -> g a
(.+.) x y = review _TmAdd (x, y)

bool :: AsBool t g a => Bool -> g a
bool = review _TmBool

infixr 3 .&&.
(.&&.) :: AsBool t g a => g a -> g a -> g a
(.&&.) x y = review _TmAnd (x, y)

infix 4 .==.
(.==.) :: AsEq t g a => g a -> g a -> g a
(.==.) x y = review _TmEq (x, y)

eAndFalse :: (AsBool t g a) => g a -> Maybe (g a)
eAndFalse =
  fmap (const $ review _TmBool False) .
  preview (_TmAnd . liftL2 (_TmBool . only False) id . _2)

eAndTrue :: (AsBool t g a) => g a -> Maybe (g a)
eAndTrue = preview (_TmAnd . liftL2 (_TmBool . only True) id . _2)

eAnd1 :: (AsBool t g a) => (g a -> Maybe (g a)) -> g a -> Maybe (g a)
eAnd1 step t =
  case preview _TmAnd t of
    Just (x, y) -> fmap (_TmAnd #) ((,) <$> step x <*> pure y)
    _ -> Nothing

-- loc just takes an inner step, preserves the location

-- eAddIntInt :: TmAdd (TmInt i) (TmInt j) = TmInt (i + j)
-- - probably with a type family to jump from PAdd to TmAdd
-- eAppAdd :: TmApp (Prim PAdd) x = Lam y (TmAdd x (Var y))
-- - actually, could get away with PAdd = Lam Int x Lam Int y (TmAdd x y)
-- - this picks up the type of PAdd straight away
