{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for types of the I language.
-}
module Type.Gen (
    genType
  , shrinkType
  , AnyType(..)
  ) where

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, Arbitrary(..))

-- local
import           Type            (Type (..))

-- | Generates types of the I language.
genType :: Gen Type
genType =
  pure TyInt

-- | Shrinks types of the I language.
shrinkType :: Type
           -> [Type]
shrinkType _ =
  []

-- | A newtype wrapped for generating types of the I language.
newtype AnyType = AnyType {
    getAnyType :: Type
  } deriving (Eq, Show)

instance Arbitrary AnyType where
  arbitrary =
    fmap AnyType genType
  shrink =
    fmap AnyType . shrinkType . getAnyType
