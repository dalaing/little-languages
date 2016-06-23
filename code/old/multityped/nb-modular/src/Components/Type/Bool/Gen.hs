module Components.Type.Bool.Gen where

import Control.Lens (preview, review)
import Control.Lens.Prism (isn't)

import Test.QuickCheck (Gen)

import Common.Type.Gen

import Components.Type.Bool.Data

genTyBool :: WithBoolType ty
         => Gen ty
genTyBool = pure (review _TyBool ())

genNotTyBool :: WithBoolType ty
             => ty
             -> Maybe (Gen ty)
genNotTyBool ty
  | isn't _TyBool ty = Just genTyBool
  | otherwise         = Nothing

shrinkTyBool :: WithBoolType ty
             => ty
             -> Maybe [ty]
shrinkTyBool =
  fmap (const []) .
  preview _TyBool

genTypeInput :: WithBoolType ty
             => GenTypeInput ty
genTypeInput =
  GenTypeInput
    [TyBase genTyBool]
    [NTyBase genNotTyBool]
    [ShrTyBase shrinkTyBool]
