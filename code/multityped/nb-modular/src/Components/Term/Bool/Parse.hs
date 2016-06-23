module Components.Term.Bool.Parse where

import Control.Lens (review)

import Text.Parser.Combinators
import Text.Parser.Token

import Common.Recursion
import Common.Term.Parse (ParseTermInput(..))

import Components.Term.Bool.Data

parseTmFalse :: ( WithBoolTerm ty tm
                , TokenParsing m
                )
             => m tm
parseTmFalse =
  review _TmFalse () <$
    symbol "false"
    <?> "false"

parseTmTrue :: ( WithBoolTerm ty tm
               , TokenParsing m
               )
            => m tm
parseTmTrue =
  review _TmTrue () <$
    symbol "true"
    <?> "true"

parseTmIf :: ( WithBoolTerm ty tm
             , TokenParsing m
             )
          => m tm
          -> m tm
parseTmIf parseTerm =
    fmap (review _TmIf) parseTmIf'
  where
    parseTmIf' =
      (,,) <$
        symbol "if" <*>
        parseTerm <*
        symbol "then" <*>
        parseTerm <*
        symbol "else" <*>
        parseTerm
        <?> "if-then-else"

parseTermInput :: WithBoolTerm ty tm
               => ParseTermInput tm
parseTermInput =
  ParseTermInput
    [ SBase parseTmFalse
    , SBase parseTmTrue
    , SRecurse parseTmIf
    ]
