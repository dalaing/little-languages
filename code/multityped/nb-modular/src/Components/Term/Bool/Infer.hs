{-# LANGUAGE FlexibleContexts #-}
module Components.Term.Bool.Infer where

import Control.Lens (preview, review)
import Control.Monad.Except (MonadError)

import Common.Recursion (MaybeStep(..))
import Common.Note
import Common.Term.Infer
import Common.Type.Error

import Components.Type.Bool.Data
import Components.Term.Bool.Data

inferTmFalse :: ( Monad m
                , WithBoolTerm ty tm
                )
             => tm
             -> Maybe (m ty)
inferTmFalse tm = do
  _ <- preview _TmFalse tm
  return . return $ review _TyBool ()

inferTmTrue :: ( Monad m
               , WithBoolTerm ty tm
               )
            => tm
            -> Maybe (m ty)
inferTmTrue tm = do
  _ <- preview _TmTrue tm
  return . return $ review _TyBool ()

inferTmIf :: ( Eq (Without ty)
             , WithoutNote ty
             , AsUnexpected e ty
             , AsExpectedEq e ty
             , MonadError e m
             , WithBoolTerm ty tm
             )
          => (tm -> m ty)
          -> tm
          -> Maybe (m ty)
inferTmIf step tm = do
  (tm1, tm2, tm3) <- preview _TmIf tm
  return $ inferTmIf' step (tm1, tm2, tm3)

inferTmIf' :: ( Eq (Without ty)
              , WithoutNote ty
              , AsUnexpected e ty
              , AsExpectedEq e ty
              , MonadError e m
              , WithBoolTerm ty tm
              )
           => (tm -> m ty)
           -> (tm, tm, tm)
           -> m ty
inferTmIf' step (tm1, tm2, tm3) = do
  ty1 <- step tm1
  expect ty1 (review _TyBool ())
  ty2 <- step tm2
  ty3 <- step tm3
  expectEq ty2 ty3
  return ty2

inferInput :: ( Eq (Without ty)
              , WithoutNote ty
              , AsUnexpected e ty
              , AsExpectedEq e ty
              , WithBoolTerm ty tm
              )
           => InferInput e ty tm
inferInput =
  InferInput
    [ MSBase inferTmFalse
    , MSBase inferTmTrue
    , MSRecurse inferTmIf
    ]
