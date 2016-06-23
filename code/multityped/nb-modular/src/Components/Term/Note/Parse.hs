{-# LANGUAGE FlexibleContexts #-}
module Components.Term.Note.Parse where

import Control.Lens (review)

import Text.Trifecta.Combinators (DeltaParsing, spanned)
import Text.Trifecta.Rendering (Span, Spanned(..))

import Common.Recursion
import Common.Term.Parse (ParseTermInput(..))

import Components.Term.Note.Data

parseTmNote :: ( WithNoteTerm Span ty tm
               , Monad m
               , DeltaParsing m
               )
            => m tm
            -> m tm
parseTmNote parseTerm = do
  (t :~ n) <- spanned parseTerm
  return $ review _TmNoted (n, t)


parseTermInput :: WithNoteTerm Span ty tm
               => ParseTermInput tm
parseTermInput =
  ParseTermInput
    [SRecurse parseTmNote]
