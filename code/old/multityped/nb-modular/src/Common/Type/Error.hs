{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Type.Error where

import Control.Monad (unless)

import Control.Lens.TH (makeClassyPrisms)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)

import Common.Note

data Unexpected ty =
  TeUnexpected ty ty
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Unexpected

data ExpectedEq ty =
  TeExpectedEq ty ty
  deriving (Eq, Ord, Show)

makeClassyPrisms ''ExpectedEq

data UnknownType n =
  TeUnknownType (Maybe n)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''UnknownType

expect :: ( Eq (Without ty)
          , WithoutNote ty
          , AsUnexpected e ty
          , MonadError e m
          )
       => ty
       -> ty
       -> m ()
expect ac ex =
  unless (stripNote ac == stripNote ex) $
    throwing _TeUnexpected (ac, ex)

expectEq :: ( Eq (Without ty)
            , WithoutNote ty
            , AsExpectedEq e ty
            , MonadError e m
            )
         => ty
         -> ty
         -> m ()
expectEq t1 t2 =
  unless (stripNote t1 == stripNote t2) $
    throwing _TeExpectedEq (t1, t2)
