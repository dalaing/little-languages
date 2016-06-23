{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module STLC.Syntax.Int where

import Data.Tagged

import Term.Component.Int

import STLC.Type

int :: WithTmInt Type f a => Int -> Tagged TyInt (f a)
int = Tagged . Term.Component.Int.int

