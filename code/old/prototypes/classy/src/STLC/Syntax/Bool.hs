{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module STLC.Syntax.Bool where

import Data.Tagged

import Term.Component.Bool

import STLC.Type

bool :: WithTmBool Type f a => Bool -> Tagged TyBool (f a)
bool = Tagged . Term.Component.Bool.bool


