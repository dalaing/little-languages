{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module STLC.Syntax.Ord where

import Data.Tagged

import Term.Component.Ord

import STLC.Type

infix 4 .<.
(.<.) :: WithTmOrd Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a) -> Tagged TyBool (f a)
(.<.) x y = retag ((Term.Component.Ord..<.) <$> x <*> y)

infix 4 .<=.
(.<=.) :: WithTmOrd Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a) -> Tagged TyBool (f a)
(.<=.) x y = retag ((Term.Component.Ord..<=.) <$> x <*> y)

infix 4 .>.
(.>.) :: WithTmOrd Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a) -> Tagged TyBool (f a)
(.>.) x y = retag ((Term.Component.Ord..>.) <$> x <*> y)

infix 4 .>=.
(.>=.) :: WithTmOrd Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a) -> Tagged TyBool (f a)
(.>=.) x y = retag ((Term.Component.Ord..>=.) <$> x <*> y)

