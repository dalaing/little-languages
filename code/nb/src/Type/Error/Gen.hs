{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for the type errors of the NB language.
-}
module Type.Error.Gen (
    genTypeError
  , shrinkTypeError
  , AnyTypeError(..)
  ) where

-- from 'base'
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, Arbitrary(..), oneof)

-- 'local'
import           Type.Error      (TypeError (..))
import           Type.Gen        (genType, genNotType, shrinkType)

-- | Generates 'Unexpected' type errors.
genTypeErrorUnexpected :: Gen TypeError
genTypeErrorUnexpected = do
  ty1 <- genType
  ty2 <- genNotType ty1
  return $ Unexpected ty1 ty2

-- | Generates 'ExpectedEq' type errors.
genTypeErrorExpectedEq :: Gen TypeError
genTypeErrorExpectedEq = do
  ty1 <- genType
  ty2 <- genNotType ty1
  return $ ExpectedEq ty1 ty2

-- | Generates type errors of the NB language.
genTypeError :: Gen TypeError
genTypeError =
  oneof [
    genTypeErrorUnexpected
  , genTypeErrorExpectedEq
  ]

-- | Shrinks 'Unexpected' type errors.
shrinkTypeErrorUnexpected :: TypeError
                          -> Maybe [TypeError]
shrinkTypeErrorUnexpected (Unexpected ty1 ty2) =
  let
    valid (Unexpected u1 u2) =
      u1 /= u2
    valid _ =
      False
  in
    Just . filter valid $
      fmap (\s1 -> Unexpected s1 ty2) (shrinkType ty1) ++
      fmap (\s2 -> Unexpected ty1 s2) (shrinkType ty2)
shrinkTypeErrorUnexpected _ =
  Nothing

-- | Shrinks 'ExpectedEq' type errors.
shrinkTypeErrorExpectedEq :: TypeError
                          -> Maybe [TypeError]
shrinkTypeErrorExpectedEq (ExpectedEq ty1 ty2) =
  let
    valid (ExpectedEq u1 u2) =
      u1 /= u2
    valid _ =
      False
  in
    Just . filter valid $
      fmap (\s1 -> ExpectedEq s1 ty2) (shrinkType ty1) ++
      fmap (\s2 -> ExpectedEq ty1 s2) (shrinkType ty2)
shrinkTypeErrorExpectedEq _ =
  Nothing

-- | The set of shrinking rules for type errors of the NB language.
shrinkTypeErrorRules :: [TypeError -> Maybe [TypeError]]
shrinkTypeErrorRules = [
    shrinkTypeErrorUnexpected
  , shrinkTypeErrorExpectedEq
  ]

-- | Shrinks type errors of the NB language.
shrinkTypeError :: TypeError
                -> [TypeError]
shrinkTypeError te =
  fromMaybe [] .
  asum .
  fmap ($ te) $
  shrinkTypeErrorRules

-- | A newtype wrapper for generating type errors of the NB language.
newtype AnyTypeError = AnyTypeError {
    getAnyTypeError :: TypeError
  } deriving (Eq, Ord, Show)

instance Arbitrary AnyTypeError where
  arbitrary =
    AnyTypeError <$> genTypeError
  shrink =
    fmap AnyTypeError .
    shrinkTypeError .
    getAnyTypeError
