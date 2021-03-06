{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Bool.Parse (
    parseTermInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)

import Component.Term.Bool (AsBoolTerm(..), WithBoolTerm)

import Common.Parse (reserveConstructors, reserveIdentifiers, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Term.Parse (ParseTermInput(..), ParseTermRule(..))

-- |
parseTmFalse :: WithBoolTerm tm
             => ParserHelperOutput
             -> Parser (tm nTy nTm a)           -- ^
parseTmFalse h =
  let
    rc = view reservedConstructor h
  in
    review _TmFalse () <$ rc "False" <?> "False"

-- |
parseTmTrue :: WithBoolTerm tm
            => ParserHelperOutput
            -> Parser (tm nTy nTm a)           -- ^
parseTmTrue h =
  let
    rc = view reservedConstructor h
  in
    review _TmTrue () <$ rc "True" <?> "True"

-- |
parseTmIf :: WithBoolTerm tm
          => ParserHelperOutput
          -> Parser (tm nTy nTm a)           -- ^
          -> Parser (tm nTy nTm a)           -- ^
parseTmIf h parseTerm =
  let
    ri = view reservedIdentifier h
  in
    fmap (review _TmIf)
      ((,,) <$
        ri "if" <*> parseTerm <*
        ri "then" <*> parseTerm <*
        ri "else" <*> parseTerm)
      <?> "if-then-else"

parseTermInput :: WithBoolTerm tm
               => ParseTermInput ty tm
parseTermInput =
  ParseTermInput
    [ ParseTermBase (reserveConstructors ["False"]) parseTmFalse
    , ParseTermBase (reserveConstructors ["True"]) parseTmTrue
    , ParseTermRecurse (reserveIdentifiers ["if", "then", "else"]) parseTmIf
    ]
