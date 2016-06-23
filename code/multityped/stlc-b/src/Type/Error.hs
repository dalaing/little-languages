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

import Loc
import Type

data TypeError l =
    TeUnexpected { actual :: Type l, expected :: Type ()}
  | TeExpectedEq (Type l) (Type l)
  | TeUnknownType (Maybe l)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TypeError

{-
teStripLoc :: TypeError l -> TypeError ()
teStripLoc (TeUnexpected ac ex) = TeUnexpected (stripLoc ac) (stripLoc ex)
teStripLoc (TeExpectedEq t1 t2) = TeExpectedEq (stripLoc t1) (stripLoc t2)
teStripLoc (TeUnknownType _) = TeUnknownType Nothing
-}

class AsUnexpected s l | s -> l where
  _Unexpected :: Prism' s (Type l, Type ())

instance AsUnexpected (TypeError l) l where
  _Unexpected = _TeUnexpected

class AsExpectedEq s l | s -> l where
  _ExpectedEq :: Prism' s (Type l, Type l)

instance AsExpectedEq (TypeError l) l where
  _ExpectedEq = _TeExpectedEq

class AsUnknownType s l | s -> l where
  _UnknownType :: Prism' s (Maybe l)

instance AsUnknownType (TypeError l) l where
  _UnknownType = _TeUnknownType

expect :: (AsUnexpected e l, MonadError e m)
       => Type l
       -> Type ()
       -> m ()
expect ac ex =
  unless (stripLoc ac == stripLoc ex) $
    throwing _Unexpected (ac, ex)

expectEq :: (AsExpectedEq e l, MonadError e m)
       => Type l
       -> Type l
       -> m ()
expectEq t1 t2 =
  unless (stripLoc t1 == stripLoc t2) $
    throwing _ExpectedEq (t1, t2)
