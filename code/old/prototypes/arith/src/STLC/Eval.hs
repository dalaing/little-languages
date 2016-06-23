{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module STLC.Eval where

import Control.Lens

import Control.Applicative

import Bound
import Bound.Name

import Test.QuickCheck

import STLC

eAppLam :: Term l n a -> Maybe (Term l n a)
eAppLam =
  fmap (\((_,e),x) -> instantiate1Name x e) .
  preview (_TmApp . liftL2 _TmLam id)
--  fmap (\(e,x) -> instantiate1Name x e) .
--  preview (_App . liftL2 (_Lam . _2) id)

-- these could be viewd as prisms...
eApp :: (Term l n a -> Maybe (Term l n a)) -> Term l n a -> Maybe (Term l n a)
eApp step t =
  case preview _TmApp t of
    Just (x, y) -> fmap (review _TmApp) ((,) <$> step x <*> pure y)

-- whnf is made from just the above rules
-- do we want mulitple step functions to be passed around (whnf and nf?)

eAddIntInt :: Term l n a -> Maybe (Term l n a)
eAddIntInt =
  fmap (\(x,y) -> review _TmInt (x + y)) .
  preview (_TmAdd . liftL2 _TmInt _TmInt)

eAdd1 :: (Term l n a -> Maybe (Term l n a)) -> Term l n a -> Maybe (Term l n a)
eAdd1 step t =
  case preview _TmAdd t of
    Just (x, y) -> fmap (review _TmAdd) ((,) <$> step x <*> pure y)

eAdd2 :: (Term l n a -> Maybe (Term l n a)) -> Term l n a -> Maybe (Term l n a)
eAdd2 step t =
  case preview _TmAdd t of
    Just (x, y) -> fmap (review _TmAdd) ((,) <$> pure x <*> step y)

eEqIntInt :: Term l n a -> Maybe (Term l n a)
eEqIntInt =
  fmap (\(x,y) -> review _TmBool (x == y)) .
  preview (_TmEq . liftL2 _TmInt _TmInt)

eEq1 :: (Term l n a -> Maybe (Term l n a)) -> Term l n a -> Maybe (Term l n a)
eEq1 step t =
  case preview _TmEq t of
    Just (x, y) -> fmap (review _TmEq) ((,) <$> step x <*> pure y)

eEq2 :: (Term l n a -> Maybe (Term l n a)) -> Term l n a -> Maybe (Term l n a)
eEq2 step t =
  case preview _TmEq t of
    Just (x, y) -> fmap (review _TmEq) ((,) <$> pure x <*> step y)

eAndFalse :: Term l n a -> Maybe (Term l n a)
eAndFalse = fmap (const $ review _TmBool False) . preview (_TmAnd . liftL2 (_TmBool . only False) id . _2)

eAndTrue :: Term l n a -> Maybe (Term l n a)
eAndTrue = preview (_TmAnd . liftL2 (_TmBool . only True) id . _2)

eAnd1 :: (Term l n a -> Maybe (Term l n a)) -> Term l n a -> Maybe (Term l n a)
eAnd1 step t =
  case preview _TmAnd t of
    Just (x, y) -> fmap (_TmAnd #) ((,) <$> step x <*> pure y)
    _ -> Nothing

data Matcher t a where
  Matcher :: Gen a -> (t -> Maybe a) -> Matcher t a
  -- potentially: add explicit nodes for Apply and Alt, work the tree later to make it go
  -- means we can gather up all of the ORs and use a oneof to wrap them all
  -- could also add a frequency constructor...

data Rule a where
  Axiom :: String -> Gen a -> Prism' a t -> (t -> a) -> Rule a
  Rule :: String -> Gen a -> (Prism' a a -> Prism' a t) -> (t -> a) -> Rule a

eAndFalse' :: Rule (Term l n a)
eAndFalse' = Axiom "eAndFalse" undefined (_TmAnd . liftL2 (_TmBool . only False) id) (const $ _TmBool # False)

eAndTrue' :: Rule (Term l n a)
eAndTrue' = Axiom "eAndTrue" undefined (_TmAnd . liftL2 (_TmBool . only True) id) snd

eAnd1' :: Rule (Term l n a)
eAnd1' = Rule "eAnd" undefined (\step -> _TmAnd . liftL2 step id) (review _TmAnd)

-- eAnd1' :: Prism' (Term l n a) (Term l n a) -> Prism' (Term l n a) (Term l n a)
-- eAnd1' step = _And . liftL2 step id . re _And
