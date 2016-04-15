{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Type.Nat.Pretty (
    prettyTypeInput
  ) where

import Control.Lens (preview)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Common.Pretty
import Component.Type.Pretty (PrettyTypeInput(..), PrettyTypeRule(..))

import Components.Type.Nat (AsNatType(..))

prettyTyNat :: AsNatType ty
             => ty
             -> Maybe Doc
prettyTyNat =
  fmap (const . reservedConstructor $ "Nat") .
  preview _TyNat

prettyTypeInput :: AsNatType ty
                => PrettyTypeInput ty
prettyTypeInput =
  PrettyTypeInput
    [PrettyTypeBase prettyTyNat]
