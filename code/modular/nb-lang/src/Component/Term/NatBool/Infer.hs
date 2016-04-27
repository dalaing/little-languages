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

import Component.Type.Nat (AsNatType(..), WithNatType)
import Component.Type.Bool (AsBoolType(..), WithBoolType)
import Component.Term.NatBool (AsNatBoolTerm(..), WithNatBoolTerm)

-- |
inferTmIsZero :: ( Eq (ty nTy)
                 , AsUnexpected e ty nTy
                 , MonadError e m
                 , WithNatBoolTerm tm
                 , WithNatType ty
                 , WithBoolType ty
                 )
              => (ty nTy -> ty nTy)
              -> (tm nTm a -> m (ty nTy))       -- ^
              -> tm nTm a                -- ^
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
inferInput :: ( Eq (ty nTy)
              , AsUnexpected e ty nTy
              , WithNatBoolTerm tm
              , WithNatType ty
              , WithBoolType ty
              )
           => InferInput r e ty nTy tm nTm a
inferInput =
  InferInput
    [InferRecurse inferTmIsZero]
