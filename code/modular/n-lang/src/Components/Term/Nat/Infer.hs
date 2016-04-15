{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Nat.Infer (
    inferInput
  ) where

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)

import Common.Type.Error.Unexpected (AsUnexpected(..), expect)
import Component.Term.Infer (InferRule(..), InferInput(..))

import Components.Term.Nat (AsNatTerm(..), WithNatTerm)
import Components.Type.Nat (AsNatType(..))

-- |
inferTmZero :: ( Monad m
               , WithNatTerm tm a
               , AsNatType ty
               )
            => tm a          -- ^
            -> Maybe (m ty) -- ^
inferTmZero =
  fmap (const . return $ review _TyNat ()) .
  preview _TmZero

-- |
inferTmSucc :: ( Eq ty
               , AsUnexpected e ty
               , MonadError e m
               , WithNatTerm tm a
               , AsNatType ty
               )
            => (tm a -> m ty)       -- ^
            -> tm a                -- ^
            -> Maybe (m ty)       -- ^
inferTmSucc infer =
    fmap inferTmSucc' .
    preview _TmSucc
  where
    inferTmSucc' tm = do
      ty <- infer tm
      expect ty (review _TyNat ())
      return $ review _TyNat ()

-- |
inferTmPred :: ( Eq ty
               , AsUnexpected e ty
               , MonadError e m
               , WithNatTerm tm a
               , AsNatType ty
               )
            => (tm a -> m ty)       -- ^
            -> tm a                -- ^
            -> Maybe (m ty)       -- ^
inferTmPred infer =
    fmap inferTmPred' .
    preview _TmPred
  where
    inferTmPred' tm = do
      ty <- infer tm
      expect ty (review _TyNat ())
      return $ review _TyNat ()

-- |
inferInput :: ( Eq ty
              , AsUnexpected e ty
              , WithNatTerm tm a
              , AsNatType ty
              )
           => InferInput e ty (tm a)
inferInput =
  InferInput
    [ InferBase inferTmZero
    , InferRecurse inferTmSucc
    , InferRecurse inferTmPred
    ]
