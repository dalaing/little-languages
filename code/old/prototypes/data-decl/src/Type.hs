module Type where

data Type = 
    TInt
  | TBool
  | TString
  | Arrow Type Type
  deriving (Eq, Ord, Show)
