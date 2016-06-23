module Components.Term.Nat.Pretty where

import Control.Lens (preview)

import Text.PrettyPrint.ANSI.Leijen

import Common.Recursion
import Common.Term.Pretty (PrettyTermInput(..))

import Components.Term.Nat.Data

prettyTmZero :: WithNatTerm ty tm
             => tm
             -> Maybe Doc
prettyTmZero =
  fmap (const . text $ "O") .
  preview _TmZero

prettyTmSucc :: WithNatTerm ty tm
             => (tm -> Doc)
             -> tm
             -> Maybe Doc
prettyTmSucc prettyTerm =
    fmap prettyTmSucc' .
    preview _TmSucc
  where
    prettyTmSucc' t =
      text "S" <+> prettyTerm t

prettyTmPred :: WithNatTerm ty tm
             => (tm -> Doc)
             -> tm
             -> Maybe Doc
prettyTmPred prettyTerm =
    fmap prettyTmPred' .
    preview _TmPred
  where
    prettyTmPred' t =
      text "pred" <+> prettyTerm t

prettyTermInput :: WithNatTerm ty tm
                => PrettyTermInput tm
prettyTermInput =
  PrettyTermInput
    [ MSBase prettyTmZero
    , MSRecurse prettyTmSucc
    , MSRecurse prettyTmPred
    ]
