{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Type.Bool.Gen (
    genTypeInput
  ) where

import           Control.Lens         (preview, review)
import Control.Lens.Prism (isn't)
import           Test.QuickCheck      (Gen)

import           Component.Type.Gen   (GenAnyTypeRule (..), GenTypeInput (..),
                                       ShrAnyTypeRule (..), GenNotTypeRule(..), ShrNotTypeRule(..))

import           Component.Type.Bool (AsBoolType (..), WithBoolType)

-- |
genAnyTyBool :: WithBoolType ty
             => Gen (ty n)
genAnyTyBool =
  pure $ review _TyBool ()

-- |
shrAnyTyBool :: WithBoolType ty
             => ty n        -- ^
             -> Maybe [ty n] -- ^
shrAnyTyBool =
  fmap (const []) .
  preview _TyBool

-- |
genNotTyBool :: WithBoolType ty
             => ty n
             -> Maybe (Gen (ty n))
genNotTyBool ty
  | isn't _TyBool ty =
    Just . return $ review _TyBool ()
  | otherwise =
    Nothing

-- |
shrNotTyBool :: WithBoolType ty
             => ty n        -- ^
             -> Maybe [ty n] -- ^
shrNotTyBool =
  fmap (const []) .
  preview _TyBool

genTypeInput :: WithBoolType ty
             => GenTypeInput ty
genTypeInput =
  GenTypeInput
    [GenAnyTypeBase genAnyTyBool]
    [ShrAnyTypeBase shrAnyTyBool]
    [GenNotTypeBase genNotTyBool]
    [ShrNotTypeBase shrNotTyBool]
