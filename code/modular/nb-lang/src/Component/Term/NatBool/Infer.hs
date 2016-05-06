{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.NatBool.Infer (
    inferInput
  ) where

import Control.Lens (review, preview)
import Control.Monad.Except (MonadError)

import Component.Type.Error.Unexpected (AsUnexpected(..), mkExpect)
import Component.Term.Infer (InferRule(..), InferInput(..))
import Extras (Eq1)

import Component.Type.Nat (AsNatType(..), WithNatType)
import Component.Type.Bool (AsBoolType(..), WithBoolType)
import Component.Term.NatBool (AsNatBoolTerm(..), WithNatBoolTerm)

-- |
inferTmIsZero :: ( Eq1 ty
                 , Eq nTy
                 , AsUnexpected e ty
                 , MonadError (e nTy String) m
                 , WithNatBoolTerm tm
                 , WithNatType ty
                 , WithBoolType ty
                 )
              => (ty nTy -> ty nTy)
              -> (tm nTy nTm a -> m (ty nTy))       -- ^
              -> tm nTy nTm a                -- ^
              -> Maybe (m (ty nTy))       -- ^
inferTmIsZero stripNote infer =
    fmap inferTmIsZero' .
    preview _TmIsZero
  where
    expect = mkExpect stripNote
    inferTmIsZero' tm = do
      ty <- infer tm
      expect ty (review _TyNat ())
      return $ review _TyBool ()

-- |
inferInput :: ( Eq1 ty
              , AsUnexpected e ty
              , WithNatBoolTerm tm
              , WithNatType ty
              , WithBoolType ty
              )
           => InferInput r e ty tm
inferInput =
  InferInput
    [InferRecurse inferTmIsZero]
