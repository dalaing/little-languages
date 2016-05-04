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
module Component.Type.Parse (
    ParseTypeRule(..)
  , ParseTypeInput(..)
  , ParseTypeOutput(..)
  , HasParseTypeOutput(..)
  , mkParseType
  , withSpan
  ) where

import Control.Applicative (empty, (<|>))
import Control.Monad (MonadPlus)
import Data.Foldable (asum)

import Control.Lens (review)
import Control.Lens.TH (makeClassy)
import Text.Parser.Token (TokenParsing, parens)
import Text.Parser.Expression (Assoc(..), Operator(..), OperatorTable, buildExpressionParser)
import Text.Trifecta.Parser (Parser)
import Text.Trifecta.Combinators (DeltaParsing, spanned)
import Text.Trifecta.Rendering (Span, Spanned(..))

import Common.Text (ExpressionInfo(..), combineTables)
import Common.Parse (ReservedWords, GetReservedWords(..), ParserHelperOutput)
import Component.Type.Note (AsNoteType(..), WithNoteType)

-- |
data ParseTypeRule ty =
    ParseTypeBase
      ReservedWords (forall n. ParserHelperOutput -> Parser (ty n))              -- ^
  | ParseTypeRecurse
      ReservedWords
      (forall n. ParserHelperOutput -> Parser (ty n) -> Parser (ty n)) -- ^
  | ParseTypeExpression
      ReservedWords
      ExpressionInfo
      (forall n. ParserHelperOutput -> Parser (ty n -> ty n -> ty n)) -- ^

instance GetReservedWords (ParseTypeRule ty) where
  reservedWords (ParseTypeBase r _) = r
  reservedWords (ParseTypeRecurse r _) = r
  reservedWords (ParseTypeExpression r _ _) = r

-- |
fixParseTypeRule :: ParserHelperOutput
                 -> Parser (ty n)
                 -> ParseTypeRule ty
                 -> Parser (ty n)
fixParseTypeRule h _ (ParseTypeBase _ x) =
  x h
fixParseTypeRule h step (ParseTypeRecurse _ x) =
  x h step
fixParseTypeRule _ _ ParseTypeExpression{} =
  empty

-- |
fixParseExprRule :: ParserHelperOutput
                 -> ParseTypeRule ty 
                 -> OperatorTable Parser (ty n)
fixParseExprRule h (ParseTypeExpression _ (ExpressionInfo a p) f) =
  [Infix (f h) a] : replicate p []
fixParseExprRule _ _ =
  []

-- |
data ParseTypeInput ty =
  ParseTypeInput [ParseTypeRule ty] -- ^

instance GetReservedWords (ParseTypeInput ty) where
  reservedWords (ParseTypeInput i) =
    foldMap reservedWords i

instance Monoid (ParseTypeInput ty) where
  mempty =
    ParseTypeInput mempty
  mappend (ParseTypeInput v1) (ParseTypeInput v2) =
    ParseTypeInput (mappend v1 v2)

-- |
data ParseTypeOutput ty =
  ParseTypeOutput {
    _parseType      :: Parser (ty Span)        -- ^
  , _typeReservedWords :: ReservedWords -- ^
  }

makeClassy ''ParseTypeOutput

-- |
withParens :: ( MonadPlus m
              , TokenParsing m
              )
           => m (ty n)         -- ^
           -> m (ty n)         -- ^
withParens p =
  parens p <|>
  p

withSpan :: ( Monad m
            , DeltaParsing m
            , WithNoteType ty
            )
         => m (ty Span)
         -> m (ty Span)
withSpan p = do
  (ty :~ s) <- spanned p
  return $ review _TyNote (s, ty)

-- |
mkParseType :: WithNoteType ty
            => ParserHelperOutput -- ^
            -> ParseTypeInput ty -- ^
            -> ParseTypeOutput ty -- ^
mkParseType h p@(ParseTypeInput i) =
  let
    parseType' =
      withSpan $
      (<|> parens parseExpr') .
      asum .
      fmap (fixParseTypeRule h parseType') $
      i
    tables =
      combineTables .
      fmap (fixParseExprRule h) $
      i
    parseExpr' =
      withSpan $
      buildExpressionParser tables parseType'
  in
    ParseTypeOutput
      parseExpr'
      (reservedWords p)
