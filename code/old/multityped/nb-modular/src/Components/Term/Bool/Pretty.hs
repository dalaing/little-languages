module Components.Term.Bool.Pretty where

import Control.Lens (preview)

import Text.PrettyPrint.ANSI.Leijen

import Common.Recursion
import Common.Term.Pretty (PrettyTermInput(..))

import Components.Term.Bool.Data

prettyTmFalse :: WithBoolTerm ty tm
              => tm
              -> Maybe Doc
prettyTmFalse =
  fmap (const . text $ "false") .
  preview _TmFalse

prettyTmTrue :: WithBoolTerm ty tm
             => tm
             -> Maybe Doc
prettyTmTrue =
  fmap (const . text $ "true") .
  preview _TmTrue

prettyTmIf :: WithBoolTerm ty tm
             => (tm -> Doc)
             -> tm
             -> Maybe Doc
prettyTmIf prettyTerm =
  fmap prettyTmIf' .
  preview _TmIf
  where
    prettyTmIf' (t1, t2, t3) =
      text "if" <+> prettyTerm t1 </>
      text "then" <+> prettyTerm t2 </>
      text "else" <+> prettyTerm t3

prettyTermInput :: WithBoolTerm ty tm
                => PrettyTermInput tm
prettyTermInput =
  PrettyTermInput
    [ MSBase prettyTmFalse
    , MSBase prettyTmTrue
    , MSRecurse prettyTmIf
    ]
