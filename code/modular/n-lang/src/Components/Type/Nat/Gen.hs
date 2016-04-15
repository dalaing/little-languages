{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Type.Nat.Gen (
    genTypeInput
  ) where

import           Control.Lens         (preview, review)
import           Test.QuickCheck      (Gen)

import           Component.Type.Gen   (GenAnyTypeRule (..), GenTypeInput (..),
                                       ShrAnyTypeRule (..))

import           Components.Type.Nat (AsNatType (..))

-- |
genTyNat :: AsNatType ty
          => Gen ty
genTyNat =
  pure $ review _TyNat ()

-- |
shrinkTyNat :: AsNatType ty
             => ty         -- ^
             -> Maybe [ty] -- ^
shrinkTyNat =
  fmap (const []) .
  preview _TyNat

genTypeInput :: AsNatType ty
             => GenTypeInput ty
genTypeInput =
  GenTypeInput
    [GenAnyTypeBase genTyNat]
    [ShrAnyTypeBase shrinkTyNat]
