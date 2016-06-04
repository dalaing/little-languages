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
  , shrinkType
  ) where

-- from 'base'
import           Data.Foldable   (asum)
import           Data.Maybe      (fromMaybe)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, oneof)

-- local
import           Type            (Type (..))

-- | Generates 'TyNat' types.
genTyNat :: Gen Type
genTyNat =
  pure TyNat

-- | Shrinks 'TyNat' types.
shrinkTyNat :: Type
            -> Maybe [Type]
shrinkTyNat TyNat =
  Just []
shrinkTyNat _ =
  Nothing

-- | Generates 'TyBool' types.
genTyBool :: Gen Type
genTyBool =
  pure TyBool

-- | Shrinks 'TyBool' types.
shrinkTyBool :: Type
             -> Maybe [Type]
shrinkTyBool TyBool =
  Just []
shrinkTyBool _ =
  Nothing

-- | Generates types of the NB language.
genType :: Gen Type
genType =
  oneof
    [ genTyNat
    , genTyBool
    ]

-- | Shrinks types of the NB language.
shrinkType :: Type
           -> [Type]
shrinkType ty =
  fromMaybe [] .
  asum .
  fmap ($ ty) $
    [ shrinkTyNat
    , shrinkTyBool
    ]
