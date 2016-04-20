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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
module Component.Type.Bool (
    BoolType(..)
  , AsBoolType(..)
  , WithBoolType
  ) where

import Control.Lens.TH (makeClassyPrisms)

-- |
data BoolType (ty :: * -> *) n =
  TyBool -- ^
  deriving (Eq, Ord, Show)

makeClassyPrisms ''BoolType

type WithBoolType ty n = AsBoolType (ty n) ty n
