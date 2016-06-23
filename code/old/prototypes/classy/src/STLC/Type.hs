{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
module STLC.Type where

import Control.Lens

import Data.Singletons.Prelude
import Data.Singletons.TH

import Type.Component.Int
import Type.Component.Bool
import Type.Component.Arrow

singletons [d|
  data Type = 
      TyInt 
    | TyBool 
    | TyArrow Type Type
    deriving (Eq, Ord, Show)
  |]

instance AsTyInt Type where
  _TyInt = prism (const TyInt) $ \t -> case t of
      TyInt -> Right ()
      _     -> Left t

instance AsTyBool Type where
  _TyBool = prism (const TyBool) $ \t -> case t of
      TyBool -> Right ()
      _      -> Left t

instance AsTyArrow Type where
  _TyArrow = prism (uncurry TyArrow) $ \t -> case t of
      TyArrow a b -> Right (a, b)
      _           -> Left t
