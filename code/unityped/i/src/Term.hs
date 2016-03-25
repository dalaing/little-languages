{-# LANGUAGE TemplateHaskell #-}
module Term where

import Control.Lens.TH

data Term = 
    TmInt Int
  | TmAdd Term Term
  | TmSub Term Term
  | TmMul Term Term
  | TmExp Term Term
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Term

size :: Term -> Int
size (TmInt _) = 1
size (TmAdd t1 t2) = 1 + size t1 + size t2
size (TmSub t1 t2) = 1 + size t1 + size t2
size (TmMul t1 t2) = 1 + size t1 + size t2
size (TmExp t1 t2) = 1 + size t1 + size t2
