{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Components.Type.Nat (
    NatType(..)
  , AsNatType(..)
  ) where

import           Control.Lens.Prism (Prism', prism)

-- |
data NatType =
  TyNat -- ^
  deriving (Eq, Ord, Show)

class AsNatType ty where
  _NatType :: Prism' ty NatType

  _TyNat :: Prism' ty ()
  _TyNat = _NatType . tyNatPrism
    where
      tyNatPrism =
        prism (const TyNat) (const . Right $ ())

instance AsNatType NatType where
  _NatType = id
