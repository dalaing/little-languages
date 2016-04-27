{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Type.Int.Pretty (
    prettyTypeInput
  ) where

import Control.Lens (preview)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Common.Pretty
import Component.Type.Pretty (PrettyTypeInput(..), PrettyTypeRule(..))

import Component.Type.Int (AsIntType(..), WithIntType)

prettyTyInt :: WithIntType ty
             => ty n
             -> Maybe Doc
prettyTyInt =
  fmap (const . reservedConstructor $ "Int") .
  preview _TyInt

prettyTypeInput :: WithIntType ty
                => PrettyTypeInput ty
prettyTypeInput =
  PrettyTypeInput
    [PrettyTypeBase prettyTyInt]
