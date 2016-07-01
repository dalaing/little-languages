{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Generators for types of the NB language.
-}
module Type.Gen (
    genType
  , genNotType
  , shrinkType
  , AnyType(..)
  ) where

-- from 'base'
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe, mapMaybe)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, Arbitrary(..), oneof)

-- local
import           Type            (Type (..))

-- | Generates 'TyNat' types.
genTyNat :: Gen Type
genTyNat =
  pure TyNat

-- | Generates 'TyBool' types.
genTyBool :: Gen Type
genTyBool =
  pure TyBool

-- | Generates types of the NB language.
genType :: Gen Type
genType =
  oneof
    [ genTyNat
    , genTyBool
    ]

-- | Shrinks 'TyNat' types.
shrinkTyNat :: Type
            -> Maybe [Type]
shrinkTyNat TyNat =
  Just []

shrinkTyNat _ =
  Nothing

-- | Shrinks 'TyBool' types.
shrinkTyBool :: Type
             -> Maybe [Type]
shrinkTyBool TyBool =
  Just []
shrinkTyBool _ =
  Nothing

-- | The set of shrinking rules for types of the NB language.
shrinkTypeRules :: [Type -> Maybe [Type]]
shrinkTypeRules = [
    shrinkTyNat
  , shrinkTyBool
  ]

-- | Shrinks types of the NB language.
shrinkType :: Type
           -> [Type]
shrinkType ty =
  fromMaybe [] .
  asum .
  fmap ($ ty) $
  shrinkTypeRules

-- | A newtype wrapper for generating types of the NB language.
newtype AnyType = AnyType {
    getAnyType :: Type
  } deriving (Eq, Show)

instance Arbitrary AnyType where
  arbitrary =
    fmap AnyType genType
  shrink =
    fmap AnyType .
    shrinkType .
    getAnyType

-- | Generates types other than 'TyNat'
genNotTypeTyNat :: Type
                -> Maybe (Gen Type)
genNotTypeTyNat TyNat =
  Nothing
genNotTypeTyNat _ =
  Just genTyNat

-- | Generates types other than 'TyBool'
genNotTypeTyBool :: Type
                 -> Maybe (Gen Type)
genNotTypeTyBool TyBool =
  Nothing
genNotTypeTyBool _ =
  Just genTyBool

-- | Generates a type different to the given type.
genNotType :: Type
           -> Gen Type
genNotType ty =
  oneof .
  mapMaybe ($ ty) $ [
    genNotTypeTyNat
  , genNotTypeTyBool
  ]

