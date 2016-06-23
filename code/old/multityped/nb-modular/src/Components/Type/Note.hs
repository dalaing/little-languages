{-# LANGUAGE FlexibleContexts #-}
module Components.Type.Note where

import Text.Trifecta.Rendering (Span)

import Common.Type

import Components.Type.Note.Data
import Components.Type.Note.Gen
import Components.Type.Note.Parse
import Components.Type.Note.Pretty

typeInput :: WithNoteType Span ty
          => TypeInput e ty
typeInput =
  TypeInput
    genTypeInput
    parseTypeInput
    prettyTypeInput
    mempty
