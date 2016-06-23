{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Type.Error where

import Control.Lens.TH
import Control.Lens.Prism
import Control.Monad.Error.Lens

import Control.Monad.Except

import Type

data TypeInfo l = TypeInfo { tiType :: Type l, tiLoc :: l}
                deriving (Eq, Ord, Show)

data TypeError l a =
    TeUnexpected { actual :: TypeInfo l, expected :: Type l}
  | TeExpectedEq (TypeInfo l) (TypeInfo l)
  | TeExpectedArr (TypeInfo l)
  | TeUnknownVar l a
  | TeUnknownType l
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TypeError

class AsUnexpected s l a | s -> l, s -> a where
  _Unexpected :: Prism' s (TypeInfo l, Type l)

instance AsUnexpected (TypeError l a) l a where
  _Unexpected = _TeUnexpected

class AsExpectedEq s l a | s -> l, s -> a where
  _ExpectedEq :: Prism' s (TypeInfo l, TypeInfo l)

instance AsExpectedEq (TypeError l a) l a where
  _ExpectedEq = _TeExpectedEq

class AsExpectedArr s l a | s -> l, s -> a where
  _ExpectedArr :: Prism' s (TypeInfo l)

instance AsExpectedArr (TypeError l a) l a where
  _ExpectedArr = _TeExpectedArr

class AsUnknownVar s l a | s -> l, s -> a where
  _UnknownVar :: Prism' s (l, a)

instance AsUnknownVar (TypeError l a) l a where
  _UnknownVar = _TeUnknownVar

class AsUnknownType s l a | s -> l, s -> a where
  _UnknownType :: Prism' s l

instance AsUnknownType (TypeError l a) l a where
  _UnknownType = _TeUnknownType

expect :: (AsUnexpected e l a, MonadError e m)
       => TypeInfo l
       -> Type l
       -> m ()
expect ac ex =
  unless (tiType ac == ex) $
    throwing _Unexpected (ac, ex)

expectEq :: (AsExpectedEq e l a, MonadError e m)
       => TypeInfo l
       -> TypeInfo l
       -> m ()
expectEq t1 t2 =
  unless (tiType t1 == tiType t2) $
    throwing _ExpectedEq (t1, t2)

-- TODO fix this up once we've weaved source locs through Type
expectArr :: (AsExpectedArr e l a, MonadError e m)
          => TypeInfo l
          -> m (TypeInfo l, TypeInfo l)
expectArr (TypeInfo (Type (TyArr t1 t2)) l) = return (TypeInfo t1 l, TypeInfo t2 l)
expectArr t = throwing _ExpectedArr t

