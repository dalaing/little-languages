{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Components.Type.Nat.Data where

import Control.Lens.TH (makeClassyPrisms)

import Common.Note

data NatType =
  TyNat
  deriving (Eq, Ord, Show)

makeClassyPrisms ''NatType

type WithNatType ty = AsNatType ty

instance WithoutNote NatType where
  type Without NatType = NatType
  stripNote = id

