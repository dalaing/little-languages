{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Type.Nat.Parse (
    parseTypeInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)

import Common.Parse (reserveConstructors, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Type.Parse (ParseTypeInput(..), ParseTypeRule(..))

import Component.Type.Nat (AsNatType(..), WithNatType)

-- |
parseTyNat :: WithNatType ty
             => ParserHelperOutput
             -> Parser (ty n)           -- ^
parseTyNat h =
  let
    rc = view reservedConstructor h
  in
    review _TyNat () <$ rc "Nat" <?> "Nat"

parseTypeInput :: WithNatType ty
               => ParseTypeInput ty
parseTypeInput =
  ParseTypeInput
    [ParseTypeBase (reserveConstructors ["Nat"]) parseTyNat]
