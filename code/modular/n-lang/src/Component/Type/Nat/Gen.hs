{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Type.Nat.Gen (
    genTypeInput
  ) where

import           Control.Lens         (preview, review)
import Control.Lens.Prism (isn't)
import           Test.QuickCheck      (Gen)

import           Component.Type.Gen   (GenAnyTypeRule (..), GenTypeInput (..),
                                       ShrAnyTypeRule (..), GenNotTypeRule(..), ShrNotTypeRule(..))

import           Component.Type.Nat (AsNatType (..), WithNatType)

-- |
genAnyTyNat :: WithNatType ty n
            => Gen (ty n)
genAnyTyNat =
  pure $ review _TyNat ()

-- |
shrAnyTyNat :: WithNatType ty n
            => ty n        -- ^
            -> Maybe [ty n] -- ^
shrAnyTyNat =
  fmap (const []) .
  preview _TyNat

-- |
genNotTyNat :: WithNatType ty n
            => ty n
            -> Maybe (Gen (ty n))
genNotTyNat ty
  | isn't _TyNat ty =
    Just . return $ review _TyNat ()
  | otherwise =
    Nothing

-- |
shrNotTyNat :: WithNatType ty n
            => ty n        -- ^
            -> Maybe [ty n] -- ^
shrNotTyNat =
  fmap (const []) .
  preview _TyNat

genTypeInput :: WithNatType ty n
             => GenTypeInput ty n
genTypeInput =
  GenTypeInput
    [GenAnyTypeBase genAnyTyNat]
    [ShrAnyTypeBase shrAnyTyNat]
    [GenNotTypeBase genNotTyNat]
    [ShrNotTypeBase shrNotTyNat]
