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

import Component.Term.Nat (AsNatTerm(..), WithNatTerm)
import Component.Type.Nat (AsNatType(..), WithNatType)

-- |
inferTmZero :: ( Monad m
               , WithNatTerm tm
               , WithNatType ty
               )
            => tm nTm a          -- ^
            -> Maybe (m (ty nTy)) -- ^
inferTmZero =
  fmap (const . return $ review _TyNat ()) .
  preview _TmZero

-- |
inferTmSucc :: ( Eq (ty nTy)
               , AsUnexpected e ty nTy
               , MonadError e m
               , WithNatTerm tm
               , WithNatType ty
               )
            => (ty nTy -> ty nTy)
            -> (tm nTm a -> m (ty nTy))       -- ^
            -> tm nTm a                -- ^
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
inferTmPred :: ( Eq (ty nTy)
               , AsUnexpected e ty nTy
               , MonadError e m
               , WithNatTerm tm
               , WithNatType ty
               )
            => (ty nTy -> ty nTy)
            -> (tm nTm a -> m (ty nTy))       -- ^
            -> tm nTm a                -- ^
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
inferInput :: ( Eq (ty nTy)
              , AsUnexpected e ty nTy
              , WithNatTerm tm
              , WithNatType ty
              )
           => InferInput r e ty nTy tm nTm a
inferInput =
  InferInput
    [ InferBase inferTmZero
    , InferRecurse inferTmSucc
    , InferRecurse inferTmPred
    ]
