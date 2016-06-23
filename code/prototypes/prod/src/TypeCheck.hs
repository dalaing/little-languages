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

data TypeError l n d a =
    Mismatch { actual :: Type l n d, expected :: Type l n d}
  | ExpectedFunction (Type l n d)
  | UnknownVar a

expect :: (Eq l, Eq n, Eq d, MonadError (TypeError l n d a) m) => Type l n d -> Type l n d -> m ()
expect ac ex =
  unless (ac == ex) .
  throwError $ Mismatch ac ex

checkAndExpect :: (Eq l, Eq d, Ord a, MonadReader (Env l d a) m, MonadError (TypeError l a d a) m) 
               => Term l a d a -> Type l a d -> m ()
checkAndExpect x ex = do
  tx <- check x
  expect tx ex

type Env l d a = M.Map a (Type l a d)

-- validation for checking, non empty list of errors?
--  something grabbing as much info as we can would be nice
--  
-- need to check pattern types
--   free vars and decls are errors
--     check ahead of time?
-- can log warnings when shadowing vars through a check on the environment
-- map
--
-- can we use cont to do bottom up type checking?
--
-- checking of primops through metadata would be nice
check :: (Eq l, Eq d, Ord a, MonadReader (Env l d a) m, MonadError (TypeError l a d a) m)
      => Term l a d a
      -> m (Type l a d)
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
  checkAndExpect x TyInt
  checkAndExpect y TyInt
  return TyInt
check (Equ x y) = do
  checkAndExpect x TyInt
  checkAndExpect y TyInt
  return TyBool
check (And x y) = do
  checkAndExpect x TyBool
  checkAndExpect y TyBool
  return TyBool
