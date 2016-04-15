{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Bool.Infer (
    inferInput
  ) where

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)

import Common.Type.Error.Unexpected (AsUnexpected(..), expect)
import Common.Type.Error.ExpectedEq (AsExpectedEq(..), expectEq)
import Component.Term.Infer (InferRule(..), InferInput(..))

import Components.Term.Bool (AsBoolTerm(..), WithBoolTerm)
import Components.Type.Bool (AsBoolType(..))

-- |
inferTmFalse :: ( Monad m
                , WithBoolTerm tm a
                , AsBoolType ty
                )
             => tm a          -- ^
             -> Maybe (m ty) -- ^
inferTmFalse =
  fmap (const . return $ review _TyBool ()) .
  preview _TmFalse

-- |
inferTmTrue :: ( Monad m
               , WithBoolTerm tm a
               , AsBoolType ty
               )
            => tm a          -- ^
            -> Maybe (m ty) -- ^
inferTmTrue =
  fmap (const . return $ review _TyBool ()) .
  preview _TmTrue

-- |
inferTmIf :: ( Eq ty
             , AsUnexpected e ty
             , AsExpectedEq e ty
             , MonadError e m
             , WithBoolTerm tm a
             , AsBoolType ty
             )
          => (tm a -> m ty)       -- ^
          -> tm a                -- ^
          -> Maybe (m ty)         -- ^
inferTmIf infer =
    fmap inferTmIf' .
    preview _TmIf
  where
    inferTmIf' (tm1, tm2, tm3) = do
      ty1 <- infer tm1
      expect ty1 (review _TyBool ())
      ty2 <- infer tm2
      ty3 <- infer tm3
      expectEq ty2 ty3
      return ty3

-- |
inferInput :: ( Eq ty
              , AsUnexpected e ty
              , AsExpectedEq e ty
              , WithBoolTerm tm a
              , AsBoolType ty
              )
           => InferInput e ty (tm a)
inferInput =
  InferInput
    [ InferBase inferTmFalse
    , InferBase inferTmTrue
    , InferRecurse inferTmIf
    ]
