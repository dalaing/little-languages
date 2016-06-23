module Components.Type.Bool.Pretty where

import Control.Lens (preview)

import Text.PrettyPrint.ANSI.Leijen

import Common.Recursion
import Common.Type.Pretty

import Components.Type.Bool.Data

prettyTyBool :: WithBoolType ty
             => ty
             -> Maybe Doc
prettyTyBool =
  fmap (const . text $ "Bool") .
  preview _TyBool

prettyTypeInput :: WithBoolType ty
                => PrettyTypeInput ty
prettyTypeInput =
  PrettyTypeInput
    [ MSBase prettyTyBool ]
