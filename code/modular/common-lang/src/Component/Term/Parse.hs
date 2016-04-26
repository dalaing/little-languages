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
data ParseTermRule ty nTy tm nTm a =
    ParseTermBase
      ReservedWords (ParserHelperOutput -> Parser (tm nTm a))              -- ^
  | ParseTermRecurse
      ReservedWords
      (ParserHelperOutput -> Parser (tm nTm a) -> Parser (tm nTm a)) -- ^
  | ParseTermExpression
      ReservedWords
      ExpressionInfo
      (ParserHelperOutput -> Parser (tm nTm a -> tm nTm a -> tm nTm a)) -- ^
  | ParseTermWithType
      ReservedWords
      (ParserHelperOutput -> Parser (ty nTy) -> Parser (tm nTm a) -> Parser (tm nTm a)) -- ^

instance GetReservedWords (ParseTermRule ty nTy tm nTm a) where
  reservedWords (ParseTermBase r _) = r
  reservedWords (ParseTermRecurse r _) = r
  reservedWords (ParseTermExpression r _ _) = r
  reservedWords (ParseTermWithType r _) = r

-- |
fixParseTermRule :: ParserHelperOutput
                 -> Parser (ty nTy)
                 -> Parser (tm nTm a)
                 -> ParseTermRule ty nTy tm nTm a
                 -> Parser (tm nTm a)
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
                 -> ParseTermRule ty nTy tm nTm a
                 -> OperatorTable Parser (tm nTm a)
fixParseExprRule h (ParseTermExpression _ (ExpressionInfo a p) f) =
  [Infix (f h) a] : replicate p []
fixParseExprRule _ _ =
  []

-- |
data ParseTermInput ty nTy tm nTm a =
  ParseTermInput [ParseTermRule ty nTy tm nTm a] -- ^

instance GetReservedWords (ParseTermInput ty nTy tm nTm a) where
  reservedWords (ParseTermInput i) =
    foldMap reservedWords i

instance Monoid (ParseTermInput ty nTy tm nTm a) where
  mempty =
    ParseTermInput mempty
  mappend (ParseTermInput v1) (ParseTermInput v2) =
    ParseTermInput (mappend v1 v2)

-- |
data ParseTermOutput tm n a =
  ParseTermOutput {
    _parseTerm      :: (Parser (tm n a) -> Parser (tm n a)) -> Parser (tm n a)        -- ^
  , _termReservedWords :: ReservedWords -- ^
  }

makeClassy ''ParseTermOutput

-- |
withParens :: ( MonadPlus m
              , TokenParsing m
              )
           => m (tm n a)         -- ^
           -> m (tm n a)         -- ^
withParens p =
  parens p <|>
  p

withSpan :: ( Monad m
            , DeltaParsing m
            , WithNoteTerm tm Span a
            )
         => m (tm Span a)
         -> m (tm Span a)
withSpan p = do
  (tm :~ s) <- spanned p
  return $ review _TmNote (s, tm)

-- |
mkParseTerm :: ParserHelperOutput -- ^
            -> ParseTypeOutput ty nTy
            -> ParseTermInput ty nTy tm nTm a -- ^
            -> ParseTermOutput tm nTm a -- ^
mkParseTerm h (ParseTypeOutput parseType _) p@(ParseTermInput i) =
  let
    parseTerm' pt =
      pt $
      withParens .
      asum .
      fmap (fixParseTermRule h (parseType id) (parseTerm' pt)) $
      i
    tables =
      combineTables .
      fmap (fixParseExprRule h) $
      i
    parseExpr' pt =
      pt $
      buildExpressionParser tables (parseTerm' pt)
  in
    ParseTermOutput
      parseExpr'
      (reservedWords p)
