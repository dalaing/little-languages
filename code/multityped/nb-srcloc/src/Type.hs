{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Type where

import Control.Lens.TH

import Loc

data Type l =
    TyBool
  | TyNat
  | TyLoc l (Type l)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Type

instance WithLoc Type where
  _Loc = _TyLoc

  stripLoc TyBool      = TyBool
  stripLoc TyNat       = TyNat
  stripLoc (TyLoc _ t) = stripLoc t
