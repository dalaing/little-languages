{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Component.Type.Error (
    TypeErrorInput(..)
  , TypeErrorOutput(..)
  , HasTypeErrorOutput(..)
  , mkTypeError
  ) where

import Control.Lens (view)
import           Control.Lens.TH           (makeClassy)

import           Component.Type (TypeOutput, HasTypeOutput(..))
import           Component.Type.Error.Pretty (PrettyTypeErrorInput(..), PrettyTypeErrorOutput(..), mkPrettyTypeError)

data TypeErrorInput e ty =
  TypeErrorInput {
    _prettyTypeErrorInput :: PrettyTypeErrorInput e ty
  }

instance Monoid (TypeErrorInput e ty) where
  mempty =
    TypeErrorInput
      mempty
  mappend (TypeErrorInput pr1) (TypeErrorInput pr2) =
    TypeErrorInput
      (mappend pr1 pr2)

data TypeErrorOutput e =
  TypeErrorOutput {
    _toePrettyTypeErrorOutput :: PrettyTypeErrorOutput e
  }

makeClassy ''TypeErrorOutput

mkTypeError :: TypeOutput ty
            -> TypeErrorInput e ty
            -> TypeErrorOutput e
mkTypeError to (TypeErrorInput pr) =
  let
    pTy = view toPrettyTypeOutput to
  in
    TypeErrorOutput
      (mkPrettyTypeError pTy pr)
