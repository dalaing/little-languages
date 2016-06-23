module Type where

data Type =
    TyInt
  | TyBool
  | TyArr Type Type
  deriving (Eq, Ord, Show)
