module Components.Term.Note.Pretty where

import Control.Lens (preview)

import Text.PrettyPrint.ANSI.Leijen

import Common.Recursion
import Common.Term.Pretty (PrettyTermInput(..))

import Components.Term.Note.Data

prettyTmNoted :: WithNoteTerm n ty tm
               => (tm -> Doc)
               -> tm
               -> Maybe Doc
prettyTmNoted prettyTerm =
    fmap prettyTmNoted' .
    preview _TmNoted
  where
    prettyTmNoted' (_, t) =
      prettyTerm t

prettyTermInput :: WithNoteTerm n ty tm
                => PrettyTermInput tm
prettyTermInput =
  PrettyTermInput
    [MSRecurse prettyTmNoted]
