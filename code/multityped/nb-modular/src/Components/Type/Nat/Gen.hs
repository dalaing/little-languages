module Components.Type.Nat.Gen where

import Control.Lens (preview, review)
import Control.Lens.Prism (isn't)

import Test.QuickCheck (Gen)

import Common.Type.Gen

import Components.Type.Nat.Data

genTyNat :: WithNatType ty
         => Gen ty
genTyNat = pure (review _TyNat ())

genNotTyNat :: WithNatType ty
             => ty
             -> Maybe (Gen ty)
genNotTyNat ty
  | isn't _TyNat ty = Just genTyNat
  | otherwise         = Nothing

shrinkTyNat :: WithNatType ty
            => ty
            -> Maybe [ty]
shrinkTyNat =
  fmap (const []) .
  preview _TyNat

genTypeInput :: WithNatType ty
             => GenTypeInput ty
genTypeInput =
  GenTypeInput
    [TyBase genTyNat]
    [NTyBase genNotTyNat]
    [ShrTyBase shrinkTyNat]
