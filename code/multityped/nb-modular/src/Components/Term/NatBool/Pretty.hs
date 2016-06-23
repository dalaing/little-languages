module Components.Term.NatBool.Pretty where

import Control.Lens (preview)

import Text.PrettyPrint.ANSI.Leijen

import Common.Recursion
import Common.Term.Pretty (PrettyTermInput(..))

import Components.Term.NatBool.Data

prettyTmIsZero :: WithNatBoolTerm ty tm
               => (tm -> Doc)
               -> tm
               -> Maybe Doc
prettyTmIsZero prettyTerm =
    fmap prettyTmIsZero' .
    preview _TmIsZero
  where
    prettyTmIsZero' t =
      text "isZero" <+> prettyTerm t

prettyTermInput :: WithNatBoolTerm ty tm
                => PrettyTermInput tm
prettyTermInput =
  PrettyTermInput
    [MSRecurse prettyTmIsZero]
