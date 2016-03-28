{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Type.Error where

import Control.Lens.TH
import Control.Lens.Prism
import Control.Monad.Error.Lens

import Control.Monad.Except

import Type

data TypeError =
    TeUnexpected { actual :: Type, expected :: Type}
  | TeExpectedEq Type Type
  | TeUnknownType
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TypeError

class AsUnexpected s where
  _Unexpected :: Prism' s (Type, Type)

instance AsUnexpected TypeError where
  _Unexpected = _TeUnexpected

class AsExpectedEq s where
  _ExpectedEq :: Prism' s (Type, Type)

instance AsExpectedEq TypeError where
  _ExpectedEq = _TeExpectedEq

class AsUnknownType s where
  _UnknownType :: Prism' s ()

instance AsUnknownType TypeError where
  _UnknownType = _TeUnknownType

expect :: (AsUnexpected e, MonadError e m)
       => Type
       -> Type
       -> m ()
expect ac ex =
  unless (ac == ex) $
    throwing _Unexpected (ac, ex)

expectEq :: (AsExpectedEq e, MonadError e m)
       => Type
       -> Type
       -> m ()
expectEq t1 t2 =
  unless (t1 == t2) $
    throwing _ExpectedEq (t1, t2)
