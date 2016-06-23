{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Nat.Infer (
    inferInput
  ) where

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)

import Component.Type.Error.Unexpected (AsUnexpected(..), mkExpect)
import Component.Term.Infer (InferRule(..), InferInput(..))
import Extras (Eq1)

import Component.Term.Nat (AsNatTerm(..), WithNatTerm)
import Component.Type.Nat (AsNatType(..), WithNatType)

-- |
inferTmZero :: ( Monad m
               , WithNatTerm tm
               , WithNatType ty
               )
            => tm nTy nTm a          -- ^
            -> Maybe (m (ty nTy)) -- ^
inferTmZero =
  fmap (const . return $ review _TyNat ()) .
  preview _TmZero

-- |
inferTmSucc :: ( Eq1 ty
               , Eq nTy
               , AsUnexpected e ty
               , MonadError (e nTy String) m
               , WithNatTerm tm
               , WithNatType ty
               )
            => (ty nTy -> ty nTy)
            -> (tm nTy nTm a -> m (ty nTy))       -- ^
            -> tm nTy nTm a                -- ^
            -> Maybe (m (ty nTy))       -- ^
inferTmSucc stripNote infer =
    fmap inferTmSucc' .
    preview _TmSucc
  where
    expect = mkExpect stripNote
    inferTmSucc' tm = do
      ty <- infer tm
      expect ty (review _TyNat ())
      return $ review _TyNat ()

-- |
inferTmPred :: ( Eq1 ty
               , Eq nTy
               , AsUnexpected e ty
               , MonadError (e nTy String) m
               , WithNatTerm tm
               , WithNatType ty
               )
            => (ty nTy -> ty nTy)
            -> (tm nTy nTm a -> m (ty nTy))       -- ^
            -> tm nTy nTm a                -- ^
            -> Maybe (m (ty nTy))       -- ^
inferTmPred stripNote infer =
    fmap inferTmPred' .
    preview _TmPred
  where
    expect = mkExpect stripNote
    inferTmPred' tm = do
      ty <- infer tm
      expect ty (review _TyNat ())
      return $ review _TyNat ()

-- |
inferInput :: ( Eq1 ty
              , AsUnexpected e ty
              , WithNatTerm tm
              , WithNatType ty
              )
           => InferInput r e ty tm
inferInput =
  InferInput
    [ InferBase inferTmZero
    , InferRecurse inferTmSucc
    , InferRecurse inferTmPred
    ]
