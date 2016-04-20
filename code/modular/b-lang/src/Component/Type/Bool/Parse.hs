{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Type.Bool.Parse (
    parseTypeInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)

import Component.Type.Bool (AsBoolType(..), WithBoolType)

import Common.Parse (reserveConstructors, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Type.Parse (ParseTypeInput(..), ParseTypeRule(..))

-- |
parseTyBool :: WithBoolType ty n
            => ParserHelperOutput
            -> Parser (ty n)           -- ^
parseTyBool h =
  let
    rc = view reservedConstructor h
  in
    review _TyBool () <$ rc "Bool" <?> "Bool"

parseTypeInput :: WithBoolType ty n
               => ParseTypeInput ty n
parseTypeInput =
  ParseTypeInput
    [ParseTypeBase (reserveConstructors ["Bool"]) parseTyBool]
