module Type.Gen where

import Test.QuickCheck (Gen, Arbitrary(..), elements)

import Type

genType :: Gen Type
genType = elements [TyBool, TyNat]

genNotType :: Type -> Gen Type
genNotType TyBool = return TyNat
genNotType TyNat = return TyBool

shrinkType :: Type -> [Type]
shrinkType = const []

newtype AnyType = AnyType { getAnyType :: Type }
                deriving (Eq, Ord, Show)

instance Arbitrary AnyType where
  arbitrary = AnyType <$> genType
  shrink = fmap AnyType . shrinkType . getAnyType
