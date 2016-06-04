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
  ) where

-- from 'QuickCheck'
import           Test.QuickCheck (Gen)

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
