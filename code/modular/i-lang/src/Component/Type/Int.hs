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
module Component.Type.Int (
    IntType(..)
  , AsIntType(..)
  , WithIntType
  ) where

import           Control.Lens.TH (makeClassyPrisms)

-- |
data IntType (ty :: * -> *) n =
  TyInt -- ^
  deriving (Eq, Ord, Show)

makeClassyPrisms ''IntType

type WithIntType ty n = AsIntType (ty n) ty n
