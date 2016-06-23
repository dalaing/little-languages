{-# LANGUAGE TemplateHaskell #-}
module Term where

import Control.Lens.TH

data Term = 
    TmZero
  | TmSucc Term
  | TmPred Term
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Term

size :: Term -> Int
size TmZero = 1
size (TmSucc t) = 1 + size t
size (TmPred t) = 1 + size t
