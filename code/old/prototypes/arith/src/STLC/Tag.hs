{-# LANGUAGE DataKinds #-}
module STLC.Tag where

import Data.Tagged
import Data.Singletons (fromSing)

import Bound
import Bound.Name

import STLC
import STLC.Type

int :: Int -> Tagged TyInt (Term l n a)
int = Tagged . Term . TmInt

bool :: Bool -> Tagged TyBool (Term l n a)
bool = Tagged . Term . TmBool

lam_ :: Eq n => Type -> n -> Term l n n -> Term l n n
lam_ t n e = Term . TmLam t $ abstract1Name n e

-- needs singletons here, to provide the type argument to lam_
lam :: Eq n => SType x -> n -> Tagged y (Term l n n) -> Tagged (TyArr x y) (Term l n n)
lam t n e = Tagged $ lam_ (fromSing t) n (untag e)

-- this will freak out unless it is created in a lam
v :: n -> Tagged t (Term l n n)
v = Tagged . Term . TmVar

-- neither of these actually stop bad types from forming
-- (.@.) :: (TyApply x y ~ z) => Tagged x (Term l n a) -> Tagged y (Term l n a) -> Tagged z (Term l n a)
-- (.@.) :: Tagged x (Term l n a) -> Tagged y (Term l n a) -> Tagged (TyApply x y) (Term l n a)

(.@.) :: Tagged (TyArr y z) (Term l n a) -> Tagged y (Term l n a) -> Tagged z (Term l n a)
(.@.) x y = Tagged . Term $ TmApp (untag $ x) (untag $ y)

(.+.) :: Tagged TyInt (Term l n a) -> Tagged TyInt (Term l n a) -> Tagged TyInt (Term l n a)
(.+.) x y = Term <$> (TmAdd <$> x <*> y)

(.==.) :: Tagged TyInt (Term l n a) -> Tagged TyInt (Term l n a) -> Tagged TyBool (Term l n a)
(.==.) x y = retag (Term <$> (TmAnd <$> x <*> y))

(.&&.) :: Tagged TyBool (Term l n a) -> Tagged TyBool (Term l n a) -> Tagged TyBool (Term l n a)
(.&&.) x y = Term <$> (TmAnd <$> x <*> y)

