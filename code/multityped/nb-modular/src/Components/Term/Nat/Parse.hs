module Components.Term.Nat.Parse where

import Control.Lens (review)

import Text.Parser.Combinators
import Text.Parser.Token

import Common.Recursion
import Common.Term.Parse (ParseTermInput(..))

import Components.Term.Nat.Data

parseTmZero :: ( WithNatTerm ty tm
                , TokenParsing m
                )
             => m tm
parseTmZero =
  review _TmZero () <$
    symbol "O"
    <?> "O"

parseTmSucc :: ( WithNatTerm ty tm
               , TokenParsing m
               )
            => m tm
            -> m tm
parseTmSucc parseTerm =
    fmap (review _TmSucc) parseTmSucc'
  where
    parseTmSucc' =
      id <$
      symbol "S" <*>
      parseTerm
      <?> "succ"

parseTmPred :: ( WithNatTerm ty tm
               , TokenParsing m
               )
            => m tm
            -> m tm
parseTmPred parseTerm =
    fmap (review _TmPred) parseTmSucc'
  where
    parseTmSucc' =
      id <$
      symbol "pred" <*>
      parseTerm
      <?> "pred"

parseTermInput :: WithNatTerm ty tm
               => ParseTermInput tm
parseTermInput =
  ParseTermInput
    [ SBase parseTmZero
    , SRecurse parseTmSucc
    , SRecurse parseTmPred
    ]
