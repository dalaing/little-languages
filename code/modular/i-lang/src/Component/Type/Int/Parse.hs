{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Type.Int.Parse (
    parseTypeInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)

import Common.Parse (reserveConstructors, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Type.Parse (ParseTypeInput(..), ParseTypeRule(..))

import Component.Type.Int (AsIntType(..), WithIntType)

-- |
parseTyInt :: WithIntType ty n
           => ParserHelperOutput
           -> Parser (ty n)           -- ^
parseTyInt h =
  let
    rc = view reservedConstructor h
  in
    review _TyInt () <$ rc "Int" <?> "Int"

parseTypeInput :: WithIntType ty n
               => ParseTypeInput ty n
parseTypeInput =
  ParseTypeInput
    [ParseTypeBase (reserveConstructors ["Int"]) parseTyInt]
