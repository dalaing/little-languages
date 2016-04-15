{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Type.Error (
    TypeError (..)
  , expect
  , expectEq
  ) where

import Control.Monad (unless)

import Control.Monad.Except (MonadError, throwError)

import Type (Type)

-- |
data TypeError =
    Unexpected Type Type -- ^
  | ExpectedEq Type Type -- ^
  | UnknownType          -- ^
  deriving (Eq, Ord, Show)

-- |
expect :: MonadError TypeError m
       => Type                   -- ^
       -> Type                   -- ^
       -> m ()                   -- ^
expect actual expected =
  unless (actual == expected) $
    throwError $ Unexpected actual expected

-- |
expectEq :: MonadError TypeError m
         => Type                   -- ^
         -> Type                   -- ^
         -> m ()                   -- ^
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwError $ ExpectedEq ty1 ty2

