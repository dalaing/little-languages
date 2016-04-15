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

import Test.QuickCheck (Gen)

import Type(Type(..))

genType :: Gen Type
genType =
  pure TyBool

shrinkType :: Type
           -> [Type]
shrinkType _ =
  []
