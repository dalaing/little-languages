{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Component.Term.STLC.Parse (
    parseTermInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Trifecta.Parser (Parser)
import Data.Constraint.Forall (ForallT)

import Common.Text (Assoc(..), ExpressionInfo(..))
import Common.Parse (reserveIdentifiers, reserveOperators, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Term.Parse (ParseTermInput(..), ParseTermRule(..))

import Component.Term.STLC (AsSTLCTerm(..), AsSTLCVar(..), WithSTLCTerm, lam_)

parseTmVar :: WithSTLCTerm tm ty
           => ParserHelperOutput
           -> Parser (tm nTy nTm String)
parseTmVar h =
  fmap (review _TmVar) (view identifier h)

parseTmLam :: ( WithSTLCTerm tm ty
              , ForallT Monad tm
              )
           => ParserHelperOutput
           -> Parser (ty nTy)
           -> Parser (tm nTy nTm String)
           -> Parser (tm nTy nTm String)
parseTmLam h parseType parseTerm = let
    ri = view reservedIdentifier h
  in
    lam_
      <$ ri "\\" <*> view identifier h
      <* ri ":" <*> parseType
      <* ri "." <*> parseTerm

-- |
parseTmApp :: WithSTLCTerm tm ty
           => ParserHelperOutput
           -> Parser (tm nTy nTm a -> tm nTy nTm a -> tm nTy nTm a) -- ^
parseTmApp h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmApp) <$
      ro "@"
      <?> "@"

parseTermInput :: ( WithSTLCTerm tm ty
                  , ForallT Monad tm
                  )
               => ParseTermInput ty tm
parseTermInput =
  ParseTermInput
    [ ParseTermBase mempty parseTmVar
    , ParseTermWithType
        (reserveIdentifiers ["\\", ":", "."])
        parseTmLam
    , ParseTermExpression
        (reserveOperators ["@"])
        (ExpressionInfo AssocRight 6)
        parseTmApp
    ]
