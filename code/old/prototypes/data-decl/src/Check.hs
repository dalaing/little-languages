module Check (
  ) where

import Term
import Type

data TypeError = T

data Ops = 
         OEqI | OAdd | OMul

opType :: Ops -> Type
opType OEqI = Arrow TypeInt (Arrow TypeInt TypeBool)
opType OEqI = Arrow TypeInt (Arrow TypeInt TypeBool)
opType OEqI = Arrow TypeInt (Arrow TypeInt TypeBool)

-- error monad, or validation?
typeCheck :: Term n a -> Either TypeError Type
typeCheck (LitI _ ) = return TypeInt
typeCheck (LitB _ ) = return TypeBool
typeCheck (LitS _ ) = return TypeString
typeCheck (Add x y) = do
  x' <- typeCheck x
  y' <- typeCheck y
  if (x == TypeInt) && (y == TypeInt)
  then return TypeInt
  else _

-- TODO infer free var type
-- bound should be tagged at the binding site, however we can probably
-- infer that too
