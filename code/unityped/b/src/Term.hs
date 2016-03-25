{-# LANGUAGE TemplateHaskell #-}
module Term where

import Control.Lens.TH

data Term =
    TmFalse
  | TmTrue
  | TmIf Term Term Term
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Term

size :: Term -> Int
size TmFalse = 1
size TmTrue = 1
size (TmIf t1 t2 t3) = 1 + size t1 + size t2 + size t3
