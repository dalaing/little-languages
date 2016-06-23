module Components.Type.Note.Pretty where

import Control.Lens (preview)

import Text.PrettyPrint.ANSI.Leijen

import Common.Recursion
import Common.Type.Pretty (PrettyTypeInput(..))

import Components.Type.Note.Data

prettyTyNote :: WithNoteType n ty
             => (ty -> Doc)
             -> ty
             -> Maybe Doc
prettyTyNote prettyType =
    fmap prettyTyNote' .
    preview _TyNoted
  where
    prettyTyNote' (_, ty) =
      prettyType ty

prettyTypeInput :: WithNoteType n ty
                => PrettyTypeInput ty
prettyTypeInput =
  PrettyTypeInput
    [ MSRecurse prettyTyNote ]
