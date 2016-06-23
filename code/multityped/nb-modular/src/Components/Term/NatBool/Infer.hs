{-# LANGUAGE FlexibleContexts #-}
module Components.Term.NatBool.Infer where

import Control.Lens (preview, review)
import Control.Monad.Except (MonadError)

import Common.Recursion (MaybeStep(..))
import Common.Note
import Common.Term.Infer
import Common.Type.Error

import Components.Type.Nat.Data
import Components.Type.Bool.Data
import Components.Term.NatBool.Data

inferTmIsZero :: ( Eq (Without ty)
                 , WithoutNote ty
                 , AsUnexpected e ty
                 , MonadError e m
                 , WithNatBoolTerm ty tm
                 )
              => (tm -> m ty)
              -> tm
              -> Maybe (m ty)
inferTmIsZero step tm = do
  tm' <- preview _TmIsZero tm
  return $ inferTmIsZero' step tm'

inferTmIsZero' :: ( Eq (Without ty)
                  , WithoutNote ty
                  , AsUnexpected e ty
                  , MonadError e m
                  , WithNatBoolTerm ty tm
                  )
               => (tm -> m ty)
               -> tm
               -> m ty
inferTmIsZero' step tm = do
  ty <- step tm
  expect ty (review _TyNat ())
  return $ review _TyBool ()

inferInput :: ( Eq (Without ty)
              , WithoutNote ty
              , AsUnexpected e ty
              , WithNatBoolTerm ty tm
              )
           => InferInput e ty tm
inferInput =
  InferInput
    [ MSRecurse inferTmIsZero ]

