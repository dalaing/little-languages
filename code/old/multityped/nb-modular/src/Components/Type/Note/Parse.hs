{-# LANGUAGE FlexibleContexts #-}
module Components.Type.Note.Parse where

import Control.Lens (review)

import Text.Trifecta.Combinators (DeltaParsing, spanned)
import Text.Trifecta.Rendering (Span, Spanned(..))

import Common.Recursion
import Common.Type.Parse

import Components.Type.Note.Data

parseTyNote :: ( WithNoteType Span ty
               , Monad m
               , DeltaParsing m
               )
            => m ty
            -> m ty
parseTyNote parseType' = do
  (t :~ n) <- spanned parseType'
  return $ review _TyNoted (n, t)


parseTypeInput :: WithNoteType Span ty
               => ParseTypeInput ty
parseTypeInput =
  ParseTypeInput
    [SRecurse parseTyNote]
