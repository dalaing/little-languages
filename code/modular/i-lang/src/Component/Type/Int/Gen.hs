{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Type.Int.Gen (
    genTypeInput
  ) where

import           Control.Lens         (preview, review)
import Control.Lens.Prism (isn't)
import           Test.QuickCheck      (Gen)

import           Component.Type.Gen   (GenAnyTypeRule (..), GenTypeInput (..),
                                       ShrAnyTypeRule (..), GenNotTypeRule(..), ShrNotTypeRule(..))

import           Component.Type.Int (AsIntType (..), WithIntType)

-- |
genAnyTyInt :: WithIntType ty n
            => Gen (ty n)
genAnyTyInt =
  pure $ review _TyInt ()

-- |
shrAnyTyInt :: WithIntType ty n
            => ty n        -- ^
            -> Maybe [ty n] -- ^
shrAnyTyInt =
  fmap (const []) .
  preview _TyInt

-- |
genNotTyInt :: WithIntType ty n
            => ty n
            -> Maybe (Gen (ty n))
genNotTyInt ty
  | isn't _TyInt ty =
    Just . return $ review _TyInt ()
  | otherwise =
    Nothing

-- |
shrNotTyInt :: WithIntType ty n
            => ty n        -- ^
            -> Maybe [ty n] -- ^
shrNotTyInt =
  fmap (const []) .
  preview _TyInt

genTypeInput :: WithIntType ty n
             => GenTypeInput ty n
genTypeInput =
  GenTypeInput
    [GenAnyTypeBase genAnyTyInt]
    [ShrAnyTypeBase shrAnyTyInt]
    [GenNotTypeBase genNotTyInt]
    [ShrNotTypeBase shrNotTyInt]
