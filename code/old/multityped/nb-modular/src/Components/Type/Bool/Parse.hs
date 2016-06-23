module Components.Type.Bool.Parse where

import Control.Lens (review)

import Text.Parser.Combinators
import Text.Parser.Token

import Common.Recursion
import Common.Type.Parse

import Components.Type.Bool.Data

parseTyBool :: ( WithBoolType ty
               , TokenParsing m
               )
            => m ty
parseTyBool =
  review _TyBool () <$
    symbol "Bool"
    <?> "Bool"

parseTypeInput :: WithBoolType ty
               => ParseTypeInput ty
parseTypeInput =
  ParseTypeInput
    [SBase parseTyBool]
