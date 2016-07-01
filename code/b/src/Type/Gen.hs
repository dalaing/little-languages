{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for types of the B language.
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

-- | Generates types of the B language.
genType :: Gen Type
genType =
  pure TyBool

-- | Shrinks types of the B language.
shrinkType :: Type
           -> [Type]
shrinkType _ =
  []

-- | A newtype wrapper for generating types of the B language.
newtype AnyType = AnyType {
    getAnyType :: Type
  } deriving (Eq, Show)

instance Arbitrary AnyType where
  arbitrary =
    fmap AnyType genType
  shrink =
    fmap AnyType . shrinkType . getAnyType
