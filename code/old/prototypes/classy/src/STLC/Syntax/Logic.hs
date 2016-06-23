{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module STLC.Syntax.Logic where

import Data.Tagged

import Term.Component.Logic

import STLC.Type

infixr 3 .&&.
(.&&.) :: WithTmLogic Type f a => Tagged TyBool (f a) -> Tagged TyBool (f a) -> Tagged TyBool (f a)
(.&&.) x y = (Term.Component.Logic..&&.) <$> x <*> y

infixr 2 .||.
(.||.) :: WithTmLogic Type f a => Tagged TyBool (f a) -> Tagged TyBool (f a) -> Tagged TyBool (f a)
(.||.) x y = (Term.Component.Logic..||.) <$> x <*> y

not :: WithTmLogic Type f a => Tagged TyBool (f a) -> Tagged TyBool (f a)
not x = Term.Component.Logic.not <$> x


