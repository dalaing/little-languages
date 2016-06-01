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
  ) where

-- from 'QuickCheck'
import           Test.QuickCheck (Gen)

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
