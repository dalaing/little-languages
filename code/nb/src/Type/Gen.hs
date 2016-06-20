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

-- |
genNotTypeTyNat :: Type
                -> Maybe (Gen Type)
genNotTypeTyNat TyNat =
  Just genTyBool
genNotTypeTyNat _ =
  Nothing

-- |
genNotTypeTyBool :: Type
                 -> Maybe (Gen Type)
genNotTypeTyBool TyBool =
  Just genTyNat
genNotTypeTyBool _ =
  Nothing

-- |
genNotType :: Type
           -> Gen Type
genNotType ty =
  oneof .
  mapMaybe ($ ty) $ [
    genNotTypeTyNat
  , genNotTypeTyBool
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

-- | A newtype wrapped for generating types of the NB language.
newtype AnyType = AnyType {
    getAnyType :: Type
  } deriving (Eq, Show)

instance Arbitrary AnyType where
  arbitrary =
    fmap AnyType genType
  shrink =
    fmap AnyType . shrinkType . getAnyType
