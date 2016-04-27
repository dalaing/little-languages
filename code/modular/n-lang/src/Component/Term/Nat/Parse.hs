{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Nat.Parse (
    parseTermInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)

import Common.Parse (reserveConstructors, reserveIdentifiers, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Term.Parse (ParseTermInput(..), ParseTermRule(..))

import Component.Term.Nat (AsNatTerm(..), WithNatTerm)

-- |
parseTmZero :: WithNatTerm tm
            => ParserHelperOutput
            -> Parser (tm n a)           -- ^
parseTmZero h =
  let
    rc = view reservedConstructor h
  in
    review _TmZero () <$ rc "O" <?> "Zero"

-- |
parseTmSucc :: WithNatTerm tm
            => ParserHelperOutput
            -> Parser (tm n a)           -- ^
            -> Parser (tm n a)           -- ^
parseTmSucc h parseTerm =
  let
    ri = view reservedIdentifier h
  in
    review _TmSucc <$
      ri "S" <*> parseTerm
      <?> "Succ"

-- |
parseTmPred :: WithNatTerm tm
            => ParserHelperOutput
            -> Parser (tm n a)           -- ^
            -> Parser (tm n a)           -- ^
parseTmPred h parseTerm =
  let
    ri = view reservedIdentifier h
  in
    review _TmPred <$
      ri "pred" <*> parseTerm
      <?> "Pred"

parseTermInput :: WithNatTerm tm
               => ParseTermInput ty nTy tm nTm a
parseTermInput =
  ParseTermInput
    [ ParseTermBase (reserveConstructors ["O"]) parseTmZero
    , ParseTermRecurse (reserveIdentifiers ["S"]) parseTmSucc
    , ParseTermRecurse (reserveIdentifiers ["pred"]) parseTmPred
    ]
