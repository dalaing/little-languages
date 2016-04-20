{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Type.Nat.Pretty (
    prettyTypeInput
  ) where

import Control.Lens (preview)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Common.Pretty
import Component.Type.Pretty (PrettyTypeInput(..), PrettyTypeRule(..))

import Component.Type.Nat (AsNatType(..), WithNatType)

prettyTyNat :: WithNatType ty n
             => ty n
             -> Maybe Doc
prettyTyNat =
  fmap (const . reservedConstructor $ "Nat") .
  preview _TyNat

prettyTypeInput :: WithNatType ty n
                => PrettyTypeInput ty n
prettyTypeInput =
  PrettyTypeInput
    [PrettyTypeBase prettyTyNat]
