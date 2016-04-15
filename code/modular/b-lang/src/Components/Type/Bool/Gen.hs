{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Type.Bool.Gen (
    genTypeInput
  ) where

import           Control.Lens         (preview, review)
import           Test.QuickCheck      (Gen)

import           Component.Type.Gen   (GenAnyTypeRule (..), GenTypeInput (..),
                                       ShrAnyTypeRule (..))

import           Components.Type.Bool (AsBoolType (..))

-- |
genTyBool :: AsBoolType ty
          => Gen ty
genTyBool =
  pure $ review _TyBool ()

-- |
shrinkTyBool :: AsBoolType ty
             => ty         -- ^
             -> Maybe [ty] -- ^
shrinkTyBool =
  fmap (const []) .
  preview _TyBool

genTypeInput :: AsBoolType ty
             => GenTypeInput ty
genTypeInput =
  GenTypeInput
    [GenAnyTypeBase genTyBool]
    [ShrAnyTypeBase shrinkTyBool]
