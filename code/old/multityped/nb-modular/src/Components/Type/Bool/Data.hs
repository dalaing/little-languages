{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Components.Type.Bool.Data where

import Control.Lens.TH (makeClassyPrisms)

import Common.Note

data BoolType =
  TyBool
  deriving (Eq, Ord, Show)

makeClassyPrisms ''BoolType

type WithBoolType ty = AsBoolType ty

instance WithoutNote BoolType where
  type Without BoolType = BoolType
  stripNote = id
