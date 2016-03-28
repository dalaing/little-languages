module Type.Error.Gen where

import Test.QuickCheck

import Type
import Type.Gen
import Type.Error

genTeUnexpected :: Type -> Gen TypeError
genTeUnexpected ex = do
  ac <- genNotType ex
  return $ TeUnexpected ac ex

-- should these be generating lists of gens?
genTeExpectedEq :: Type -> Gen TypeError
genTeExpectedEq ty = oneof [g1, g2, g3]
  where
    g1 = do
      tyOther <- genNotType ty
      return $ TeExpectedEq ty tyOther
    g2 = do
      tyOther <- genNotType ty
      return $ TeExpectedEq tyOther ty
    g3 = do
      ty1 <- genType
      ty2 <- genNotType ty1
      return $ TeExpectedEq ty1 ty2

genTypeError :: Type -> Gen TypeError
genTypeError ty =
  oneof .
  map ($ ty) $ [
    genTeUnexpected
  , genTeExpectedEq
  ]
