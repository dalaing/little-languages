{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.NatBool.Parse (
    parseTermInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)

import Common.Parse (reserveIdentifiers, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Term.Parse (ParseTermInput(..), ParseTermRule(..))

import Component.Term.NatBool (AsNatBoolTerm(..), WithNatBoolTerm)

-- |
parseTmIsZero :: WithNatBoolTerm tm
              => ParserHelperOutput
              -> Parser (tm nTy nTm a)           -- ^
              -> Parser (tm nTy nTm a)           -- ^
parseTmIsZero h parseTerm =
  let
    ri = view reservedIdentifier h
  in
    review _TmIsZero <$
      ri "isZero" <*> parseTerm
      <?> "isZero"

parseTermInput :: WithNatBoolTerm tm
               => ParseTermInput ty tm 
parseTermInput =
  ParseTermInput
    [ParseTermRecurse (reserveIdentifiers ["isZero"]) parseTmIsZero]
