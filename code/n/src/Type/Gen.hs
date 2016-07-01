{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for types of the N language.
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

-- | Generates types of the N language.
genType :: Gen Type
genType =
  pure TyNat

-- | Shrinks types of the N language.
shrinkType :: Type
           -> [Type]
shrinkType _ =
  []

-- | A newtype wrapper for generating types of the N language.
newtype AnyType = AnyType {
    getAnyType :: Type
  } deriving (Eq, Show)

instance Arbitrary AnyType where
  arbitrary =
    fmap AnyType genType
  shrink =
    fmap AnyType . shrinkType . getAnyType
