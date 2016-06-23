{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Type.Bool.Pretty (
    prettyTypeInput
  ) where

import Control.Lens (preview)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Common.Pretty
import Component.Type.Pretty (PrettyTypeInput(..), PrettyTypeRule(..))

import Component.Type.Bool (AsBoolType(..), WithBoolType)

prettyTyBool :: WithBoolType ty
             => ty n
             -> Maybe Doc
prettyTyBool =
  fmap (const . reservedConstructor $ "Bool") .
  preview _TyBool

prettyTypeInput :: WithBoolType ty
                => PrettyTypeInput ty
prettyTypeInput =
  PrettyTypeInput
    [PrettyTypeBase prettyTyBool]
