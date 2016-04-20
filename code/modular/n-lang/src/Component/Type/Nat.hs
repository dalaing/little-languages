{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
module Component.Type.Nat (
    NatType(..)
  , AsNatType(..)
  , WithNatType
  ) where

import           Control.Lens.TH (makeClassyPrisms)

-- |
data NatType (ty :: * -> *) n =
  TyNat -- ^
  deriving (Eq, Ord, Show)

makeClassyPrisms ''NatType

type WithNatType ty n = AsNatType (ty n) ty n
