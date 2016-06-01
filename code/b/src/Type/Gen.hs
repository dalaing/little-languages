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
  ) where

-- from 'QuickCheck'
import           Test.QuickCheck (Gen)

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
