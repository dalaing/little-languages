{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Type.Bool.Parse (
    parseTypeInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)

import Components.Type.Bool (AsBoolType(..))

import Common.Parse (reserveConstructors, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Type.Parse (ParseTypeInput(..), ParseTypeRule(..))

-- |
parseTyBool :: AsBoolType ty
             => ParserHelperOutput
             -> Parser ty           -- ^
parseTyBool h =
  let
    rc = view reservedConstructor h
  in
    review _TyBool () <$ rc "Bool" <?> "Bool"

parseTypeInput :: AsBoolType ty
               => ParseTypeInput ty
parseTypeInput =
  ParseTypeInput
    [ParseTypeBase (reserveConstructors ["Bool"]) parseTyBool]
