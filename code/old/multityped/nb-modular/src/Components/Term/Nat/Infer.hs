{-# LANGUAGE FlexibleContexts #-}
module Components.Term.Nat.Infer where

import Control.Lens (preview, review)
import Control.Monad.Except (MonadError)

import Common.Recursion (MaybeStep(..))
import Common.Note
import Common.Term.Infer
import Common.Type.Error

import Components.Type.Nat.Data
import Components.Term.Nat.Data

inferTmZero :: ( Monad m
               , WithNatTerm ty tm
               )
            => tm
            -> Maybe (m ty)
inferTmZero tm = do
  _ <- preview _TmZero tm
  return . return $ review _TyNat ()

inferTmSucc :: ( Eq (Without ty)
               , WithoutNote ty
               , AsUnexpected e ty
               , MonadError e m
               , WithNatTerm ty tm
               )
            => (tm -> m ty)
            -> tm
            -> Maybe (m ty)
inferTmSucc step tm = do
  tm' <- preview _TmSucc tm
  return $ inferTmSucc' step tm'

inferTmSucc' :: ( Eq (Without ty)
                , WithoutNote ty
                , AsUnexpected e ty
                , MonadError e m
                , WithNatTerm ty tm
                )
             => (tm -> m ty)
             -> tm
             -> m ty
inferTmSucc' step tm = do
  ty <- step tm
  expect ty (review _TyNat ())
  return $ review _TyNat ()

inferTmPred :: ( Eq (Without ty)
               , WithoutNote ty
               , AsUnexpected e ty
               , MonadError e m
               , WithNatTerm ty tm
               )
            => (tm -> m ty)
            -> tm
            -> Maybe (m ty)
inferTmPred step tm = do
  tm' <- preview _TmPred tm
  return $ inferTmPred' step tm'

inferTmPred' :: ( Eq (Without ty)
                , WithoutNote ty
                , AsUnexpected e ty
                , MonadError e m
                , WithNatTerm ty tm
                )
             => (tm -> m ty)
             -> tm
             -> m ty
inferTmPred' step tm = do
  ty <- step tm
  expect ty (review _TyNat ())
  return $ review _TyNat ()

inferInput :: ( Eq (Without ty)
              , WithoutNote ty
              , AsUnexpected e ty
              , WithNatTerm ty tm
              )
           => InferInput e ty tm
inferInput =
  InferInput
    [ MSBase inferTmZero
    , MSRecurse inferTmSucc
    , MSRecurse inferTmPred
    ]
