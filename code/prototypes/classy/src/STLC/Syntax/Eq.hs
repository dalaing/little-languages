{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module STLC.Syntax.Eq where

import Data.Tagged

import Term.Component.Eq

import STLC.Type

infix 4 .==.
(.==.) :: WithTmEq Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a) -> Tagged TyBool (f a)
(.==.) x y = retag ((Term.Component.Eq..==.) <$> x <*> y)

infix 4 ./=.
(./=.) :: WithTmEq Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a) -> Tagged TyBool (f a)
(./=.) x y = retag ((Term.Component.Eq../=.) <$> x <*> y)

