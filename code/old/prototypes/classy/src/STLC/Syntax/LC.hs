{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module STLC.Syntax.LC where

import Data.Tagged
import Data.Singletons (fromSing)

import Term.Component.LC

import STLC.Type

v :: WithTmLc n Type f a => a -> Tagged t (f n Type a)
v = Tagged . Term.Component.LC.v

infixr 0 .!.
(.!.) :: (WithTmLc n Type f n, Monad (f n Type), Eq n) => SType x -> n -> Tagged y (f n Type n) -> Tagged (TyArrow x y) (f n Type n)
(.!.) t n e= Tagged $ lam (fromSing t) n (untag e)

infixl 9 .@.
(.@.) :: (WithTmLc n Type f a) => Tagged (TyArrow x y ) (f n Type a) -> Tagged x (f n Type a) -> Tagged y (f n Type a)
(.@.) f x = Tagged ((Term.Component.LC..@.) (untag f) (untag x))

