{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Component.Term.Parse (
    ParseTermRule(..)
  , ParseTermInput(..)
  , ParseTermOutput(..)
  , HasParseTermOutput(..)
  , mkParseTerm
  , withSpan
  ) where

import Control.Applicative (empty, (<|>))
import Control.Monad (MonadPlus)
import Data.Foldable (asum)

import Control.Lens.TH (makeClassy)
import Control.Lens (review)
import Text.Parser.Expression (Assoc(..), Operator(..), OperatorTable, buildExpressionParser)
import Text.Parser.Token (TokenParsing, parens)
import Text.Trifecta.Parser (Parser)
import Text.Trifecta.Combinators (DeltaParsing, spanned)
import Text.Trifecta.Rendering (Span, Spanned(..))

import Common.Text (ExpressionInfo(..), combineTables)
import Common.Parse (ReservedWords, GetReservedWords(..), ParserHelperOutput)
import Component.Type.Parse (ParseTypeOutput(..))
import Component.Term.Note (AsNoteTerm(..), WithNoteTerm)

-- |
data ParseTermRule ty tm =
    ParseTermBase
      ReservedWords (forall nTy nTm. ParserHelperOutput -> Parser (tm nTy nTm String))              -- ^
  | ParseTermRecurse
      ReservedWords
      (forall nTy nTm. ParserHelperOutput -> Parser (tm nTy nTm String) -> Parser (tm nTy nTm String)) -- ^
  | ParseTermExpression
      ReservedWords
      ExpressionInfo
      (forall nTy nTm. ParserHelperOutput -> Parser (tm nTy nTm String -> tm nTy nTm String -> tm nTy nTm String)) -- ^
  | ParseTermWithType
      ReservedWords
      (forall nTy nTm. ParserHelperOutput -> Parser (ty nTy) -> Parser (tm nTy nTm String) -> Parser (tm nTy nTm String)) -- ^

instance GetReservedWords (ParseTermRule ty tm) where
  reservedWords (ParseTermBase r _) = r
  reservedWords (ParseTermRecurse r _) = r
  reservedWords (ParseTermExpression r _ _) = r
  reservedWords (ParseTermWithType r _) = r

-- |
fixParseTermRule :: ParserHelperOutput
                 -> Parser (ty nTy)
                 -> Parser (tm nTy nTm String)
                 -> ParseTermRule ty tm
                 -> Parser (tm nTy nTm String)
fixParseTermRule h _ _ (ParseTermBase _ f) =
  f h
fixParseTermRule h _ parseTerm (ParseTermRecurse _ f) =
  f h parseTerm
fixParseTermRule _ _ _ ParseTermExpression{} =
  empty
fixParseTermRule h parseType parseTerm (ParseTermWithType _ f) =
  f h parseType parseTerm

-- |
fixParseExprRule :: ParserHelperOutput
                 -> ParseTermRule ty tm
                 -> OperatorTable Parser (tm nTy nTm String)
fixParseExprRule h (ParseTermExpression _ (ExpressionInfo a p) f) =
  [Infix (f h) a] : replicate p []
fixParseExprRule _ _ =
  []

-- |
data ParseTermInput ty tm =
  ParseTermInput [ParseTermRule ty tm] -- ^

instance GetReservedWords (ParseTermInput ty tm) where
  reservedWords (ParseTermInput i) =
    foldMap reservedWords i

instance Monoid (ParseTermInput ty tm) where
  mempty =
    ParseTermInput mempty
  mappend (ParseTermInput v1) (ParseTermInput v2) =
    ParseTermInput (mappend v1 v2)

-- |
data ParseTermOutput tm =
  ParseTermOutput {
    _parseTerm      :: Parser (tm Span Span String)        -- ^
  , _termReservedWords :: ReservedWords -- ^
  }

makeClassy ''ParseTermOutput

-- |
withParens :: ( MonadPlus m
              , TokenParsing m
              )
           => m (tm nTy nTm a)         -- ^
           -> m (tm nTy nTm a)         -- ^
withParens p =
  parens p <|>
  p

withSpan :: ( Monad m
            , DeltaParsing m
            , WithNoteTerm tm
            )
         => m (tm nTy Span a)
         -> m (tm nTy Span a)
withSpan p = do
  (tm :~ s) <- spanned p
  return $ review _TmNote (s, tm)

-- |
mkParseTerm :: WithNoteTerm tm
            => ParserHelperOutput -- ^
            -> ParseTypeOutput ty
            -> ParseTermInput ty tm -- ^
            -> ParseTermOutput tm -- ^
mkParseTerm h (ParseTypeOutput parseType _) p@(ParseTermInput i) =
  let
    parseTerm' =
      withSpan $
      (<|> parens parseExpr') .
      asum .
      fmap (fixParseTermRule h parseType parseTerm') $
      i
    tables =
      combineTables .
      fmap (fixParseExprRule h) $
      i
    parseExpr' =
      withSpan $
      buildExpressionParser tables parseTerm'
  in
    ParseTermOutput
      parseExpr'
      (reservedWords p)
