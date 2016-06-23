{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Type where

import Control.Lens.TH

import Loc

data Type l =
    TyBool
  | TyArr (Type l) (Type l)
  | TyLoc l (Type l)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Type

instance WithLoc (Type l) where
  type Loc (Type l) = l
  type Without (Type l) = Type ()

  _Loc = _TyLoc

  stripLoc TyBool        = TyBool
  stripLoc (TyArr t1 t2) = TyArr (stripLoc t1) (stripLoc t2)
  stripLoc (TyLoc _ t)   = stripLoc t
