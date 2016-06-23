module Components.Type.Nat.Pretty where

import Control.Lens (preview)

import Text.PrettyPrint.ANSI.Leijen

import Common.Recursion
import Common.Type.Pretty

import Components.Type.Nat.Data

prettyTyNat :: WithNatType ty
            => ty
            -> Maybe Doc
prettyTyNat =
  fmap (const . text $ "Nat") .
  preview _TyNat

prettyTypeInput :: WithNatType ty
                => PrettyTypeInput ty
prettyTypeInput =
  PrettyTypeInput
    [ MSBase prettyTyNat ]
