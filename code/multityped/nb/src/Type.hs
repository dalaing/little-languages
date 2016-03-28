{-# LANGUAGE TemplateHaskell #-}
module Type where

import Control.Lens.TH

data Type =
    TyBool
  | TyNat
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Type

