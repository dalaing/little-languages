{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Bool.Infer (
    inferInput
  ) where

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)

import Component.Type.Error.Unexpected (AsUnexpected(..), mkExpect)
import Component.Type.Error.ExpectedEq (AsExpectedEq(..), mkExpectEq)
import Component.Term.Infer (InferRule(..), InferInput(..))
import Extras (Eq1)

import Component.Term.Bool (AsBoolTerm(..), WithBoolTerm)
import Component.Type.Bool (AsBoolType(..), WithBoolType)

-- |
inferTmFalse :: ( Monad m
                , WithBoolTerm tm
                , WithBoolType ty
                )
             => tm nTy nTm a          -- ^
             -> Maybe (m (ty nTy)) -- ^
inferTmFalse =
  fmap (const . return $ review _TyBool ()) .
  preview _TmFalse

-- |
inferTmTrue :: ( Monad m
               , WithBoolTerm tm
               , WithBoolType ty
               )
            => tm nTy nTm a          -- ^
            -> Maybe (m (ty nTy)) -- ^
inferTmTrue =
  fmap (const . return $ review _TyBool ()) .
  preview _TmTrue

-- |
inferTmIf :: ( Eq1 ty
             , Eq nTy
             , AsUnexpected e ty
             , AsExpectedEq e ty
             , MonadError (e nTy String) m
             , WithBoolTerm tm
             , WithBoolType ty
             )
          => (ty nTy -> ty nTy)
          -> (tm nTy nTm a -> m (ty nTy))       -- ^
          -> tm nTy nTm a                -- ^
          -> Maybe (m (ty nTy))         -- ^
inferTmIf stripNote infer =
    fmap inferTmIf' .
    preview _TmIf
  where
    expect = mkExpect stripNote
    expectEq = mkExpectEq stripNote
    inferTmIf' (tm1, tm2, tm3) = do
      ty1 <- infer tm1
      expect ty1 (review _TyBool ())
      ty2 <- infer tm2
      ty3 <- infer tm3
      expectEq ty2 ty3
      return ty3

-- |
inferInput :: ( Eq1 ty
              , AsUnexpected e ty
              , AsExpectedEq e ty
              , WithBoolTerm tm
              , WithBoolType ty
              )
           => InferInput r e ty tm
inferInput =
  InferInput
    [ InferBase inferTmFalse
    , InferBase inferTmTrue
    , InferRecurse inferTmIf
    ]
