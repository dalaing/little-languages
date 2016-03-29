module Type.Gen where

import Text.Trifecta.Rendering (Span)

import Test.QuickCheck (Gen, Arbitrary(..), elements)

import Loc
import Type

genType :: Gen (Type l)
genType = elements [TyBool, TyNat]

genNotType :: Type l -> Gen (Type l)
genNotType TyBool = return TyNat
genNotType TyNat = return TyBool
genNotType (TyLoc _ t) = genNotType t

shrinkType :: Type l -> [Type l]
shrinkType = const []

newtype AnyType = AnyType { getAnyType :: Type Span }
                deriving (Show)

instance Eq AnyType where
  AnyType x == AnyType y =
    stripLoc x == stripLoc y

instance Ord AnyType where
  compare (AnyType x) (AnyType y) =
    compare (stripLoc x) (stripLoc y)

instance Arbitrary AnyType where
  arbitrary = AnyType <$> genType
  shrink = fmap AnyType . shrinkType . getAnyType
