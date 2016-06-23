module Components.Term.NatBool.Parse where

import Control.Lens (review)

import Text.Parser.Combinators
import Text.Parser.Token

import Common.Recursion
import Common.Term.Parse (ParseTermInput(..))

import Components.Term.NatBool.Data

parseTmIsZero :: ( WithNatBoolTerm ty tm
                 , TokenParsing m
                 )
              => m tm
              -> m tm
parseTmIsZero parseTerm =
    fmap (review _TmIsZero) parseTmIsZero'
  where
    parseTmIsZero' =
      id <$
      symbol "isZero" <*>
      parseTerm
      <?> "isZero"

parseTermInput :: WithNatBoolTerm ty tm
               => ParseTermInput tm
parseTermInput =
  ParseTermInput
    [SRecurse parseTmIsZero]
