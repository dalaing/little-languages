{-# LANGUAGE FlexibleContexts #-}
module TypeCheck where

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except

import Bound
import Bound.Scope
import Bound.Name

import Term
import Type

data TypeError a =
    Mismatch { actual :: Type, expected :: Type}
  | ExpectedFunction Type
  | UnknownVar a

expect :: (MonadError (TypeError a) m) => Type -> Type -> m ()
expect ac ex =
  unless (ac == ex) .
  throwError $ Mismatch ac ex

type Env a = M.Map a Type

-- validation for checking, non empty list of errors
check :: (MonadReader (Env a) m, MonadError (TypeError a) m, Ord a)
      => Term l a a
      -> m Type
check (Var a) = do
  t <- asks (M.lookup a)
  case t of
    Just x -> return x
    Nothing -> throwError $ UnknownVar a
check (Lam t s) =
  let
    v = head . map name . bindings $ s
    e = instantiate1Name (Var v) s
  in
    local (M.insert v t) (check e)
check (App f x) = do
  tf <- check f
  tx <- check x
  case tf of
    TyArr a b -> do
      expect a tx
      return b
    _ -> throwError $ ExpectedFunction tf
check (Loc l e) =
  check e
check (Add x y) = do
  tx <- check x
  expect tx TyInt
  ty <- check y
  expect ty TyInt
  return TyInt
check (Equ x y) = do
  tx <- check x
  expect tx TyInt
  ty <- check y
  expect ty TyInt
  return TyBool
check (And x y) = do
  tx <- check x
  expect tx TyBool
  ty <- check y
  expect ty TyBool
  return TyBool
