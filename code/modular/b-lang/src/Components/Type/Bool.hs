{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Type.Bool (
    BoolType(..)
  , AsBoolType(..)
  ) where

import           Control.Lens.Prism (Prism', prism)

-- |
data BoolType =
  TyBool -- ^
  deriving (Eq, Ord, Show)

class AsBoolType ty where
  _BoolType :: Prism' ty BoolType

  _TyBool :: Prism' ty ()
  _TyBool = _BoolType . tyBoolPrism
    where
      tyBoolPrism =
        prism (const TyBool) (const . Right $ ())

instance AsBoolType BoolType where
  _BoolType = id
