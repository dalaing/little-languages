{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Term.STLC.Parse (
    parseTermInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)
import Bound (abstract1)

import Common.Text (Assoc(..), ExpressionInfo(..))
import Common.Parse (reserveIdentifiers, reserveOperators, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Term.Parse (ParseTermInput(..), ParseTermRule(..))
import Component.Type.Parse (ParseTypeOutput(..))

import Component.Term.STLC (AsSTLCTerm(..), AsSTLCVar(..), WithSTLCTerm)

parseTmVar :: WithSTLCTerm tm ty nTy nTm String
           => ParserHelperOutput
           -> Parser (tm nTm String)
parseTmVar h =
  fmap (review _TmVar) (view identifier h)

parseTmLam :: ( WithSTLCTerm tm ty nTy nTm String
              , Monad (tm nTm)
              )
           => ParserHelperOutput
           -> Parser (ty nTy)
           -> Parser (tm nTm String)
           -> Parser (tm nTm String)
parseTmLam h parseType parseTerm = let
    ri = view reservedIdentifier h
  in
    (\v t s -> review _TmLam (v, t, abstract1 v s))
      <$ ri "\\" <*> view identifier h
      <* ri ":" <*> parseType
      <* ri "->" <*> parseTerm

-- |
parseTmApp :: WithSTLCTerm tm ty nTy nTm String
           => ParserHelperOutput
           -> Parser (tm nTm String -> tm nTm String -> tm nTm String) -- ^
parseTmApp h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmApp) <$
      ro "@"
      <?> "@"

parseTermInput :: ( WithSTLCTerm tm ty nTy nTm String
                  , Monad (tm nTm)
                  )
               => ParseTermInput ty nTy tm nTm String
parseTermInput =
  ParseTermInput
    [ ParseTermBase mempty parseTmVar
    , ParseTermWithType
        (reserveIdentifiers ["\\", ":", "->"])
        parseTmLam
    , ParseTermExpression
        (reserveOperators ["@"])
        (ExpressionInfo AssocRight 6)
        parseTmApp
    ]
