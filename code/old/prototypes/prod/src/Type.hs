module Type where

import Bound
import Bound.Name

import Decl

data Type l n d =
    TyInt
  | TyBool
  | TyArr (Type l n d) (Type l n d)
  | TyDecl d
  deriving (Eq, Ord, Show)

