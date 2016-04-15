{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Nat.Parse (
    parseTermInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)

import Common.Parse (reserveConstructors, reserveIdentifiers, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Term.Parse (ParseTermInput(..), ParseTermRule(..))

import Components.Term.Nat (AsNatTerm(..), WithNatTerm)

-- |
parseTmZero :: WithNatTerm tm a
            => ParserHelperOutput
            -> Parser (tm a)           -- ^
parseTmZero h =
  let
    rc = view reservedConstructor h
  in
    review _TmZero () <$ rc "O" <?> "Zero"

-- |
parseTmSucc :: WithNatTerm tm a
            => ParserHelperOutput
            -> Parser (tm a)           -- ^
            -> Parser (tm a)           -- ^
parseTmSucc h parseTerm =
  let
    ri = view reservedIdentifier h
  in
    review _TmSucc <$
      ri "S" <*> parseTerm
      <?> "Succ"

-- |
parseTmPred :: WithNatTerm tm a
            => ParserHelperOutput
            -> Parser (tm a)           -- ^
            -> Parser (tm a)           -- ^
parseTmPred h parseTerm =
  let
    ri = view reservedIdentifier h
  in
    review _TmPred <$
      ri "pred" <*> parseTerm
      <?> "Pred"

parseTermInput :: WithNatTerm tm a
               => ParseTermInput tm a
parseTermInput =
  ParseTermInput
    [ ParseTermBase (reserveConstructors ["O"]) parseTmZero
    , ParseTermRecurse (reserveIdentifiers ["S"]) parseTmSucc
    , ParseTermRecurse (reserveIdentifiers ["pred"]) parseTmPred
    ]
