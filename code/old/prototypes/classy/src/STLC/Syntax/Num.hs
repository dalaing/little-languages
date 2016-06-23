{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module STLC.Syntax.Num where

import Data.Tagged

import Term.Component.Num

import STLC.Type

infixl 6 .+.
(.+.) :: WithTmNum Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a) -> Tagged TyInt (f a)
(.+.) x y = (Term.Component.Num..+.) <$> x <*> y

infixl 6 .-.
(.-.) :: WithTmNum Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a) -> Tagged TyInt (f a)
(.-.) x y = (Term.Component.Num..-.) <$> x <*> y

infixl 7 .*.
(.*.) :: WithTmNum Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a) -> Tagged TyInt (f a)
(.*.) x y = (Term.Component.Num..*.) <$> x <*> y

neg :: WithTmNum Type f a => Tagged TyInt (f a) -> Tagged TyInt (f a)
neg x = Term.Component.Num.neg <$> x

