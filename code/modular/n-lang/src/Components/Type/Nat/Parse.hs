{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Type.Nat.Parse (
    parseTypeInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)

import Common.Parse (reserveConstructors, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Type.Parse (ParseTypeInput(..), ParseTypeRule(..))

import Components.Type.Nat (AsNatType(..))

-- |
parseTyNat :: AsNatType ty
             => ParserHelperOutput
             -> Parser ty           -- ^
parseTyNat h =
  let
    rc = view reservedConstructor h
  in
    review _TyNat () <$ rc "Nat" <?> "Nat"

parseTypeInput :: AsNatType ty
               => ParseTypeInput ty
parseTypeInput =
  ParseTypeInput
    [ParseTypeBase (reserveConstructors ["Nat"]) parseTyNat]
