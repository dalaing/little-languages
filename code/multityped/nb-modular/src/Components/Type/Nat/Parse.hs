module Components.Type.Nat.Parse where

import Control.Lens (review)

import Text.Parser.Combinators
import Text.Parser.Token

import Common.Recursion
import Common.Type.Parse

import Components.Type.Nat.Data

parseTyNat :: ( WithNatType ty
              , TokenParsing m
              )
           => m ty
parseTyNat =
  review _TyNat () <$
    symbol "Nat"
    <?> "Nat"

parseTypeInput :: WithNatType ty
               => ParseTypeInput ty
parseTypeInput =
  ParseTypeInput
    [SBase parseTyNat]
