{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Term.Parse where

import Control.Lens (review)
import Data.Foldable (asum)

import Control.Monad.Except

import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS
import Text.Trifecta
import Text.Trifecta.Delta
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Term

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "nb" lower alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["O", "S", "pred", "true", "false", "if", "then", "else", "isZero"]

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseZero :: (Monad m, TokenParsing m) => m (Term l)
parseZero = review _TmZero () <$ reserved "O" <?> "O"

parseSucc :: (Monad m, TokenParsing m) => m (Term l) -> m (Term l)
parseSucc p = review _TmSucc <$ reserved "S" <*> p <?> "succ"

parsePred :: (Monad m, TokenParsing m) => m (Term l) -> m (Term l)
parsePred p = review _TmPred <$ reserved "pred" <*> p <?> "pred"

parseTrue :: (Monad m, TokenParsing m) => m (Term l)
parseTrue = review _TmTrue () <$ reserved "true" <?> "true"

parseFalse :: (Monad m, TokenParsing m) => m (Term l)
parseFalse = review _TmFalse () <$ reserved "false" <?> "false"

parseIf :: (Monad m, TokenParsing m) => m (Term l) -> m (Term l)
parseIf p = fmap (review _TmIf) ((,,) <$ reserved "if" <*> p <* reserved "then" <*> p <* reserved "else" <*> p <?> "if-then-else")

parseIsZero :: (Monad m, TokenParsing m) => m (Term l) -> m (Term l)
parseIsZero p = review _TmIsZero <$ reserved "isZero" <*> p <?> "isZero"

parseTermF :: (Monad m, DeltaParsing m) => (m (Term l) -> m (Term l)) -> m (Term l)
parseTermF p = p . asum $ [
      parseZero
    , parseSucc child
    , parsePred child
    , parseTrue
    , parseFalse
    , parseIf child
    , parseIsZero child
    ]
  where
    child = parseTermF p

parseTmLoc :: (Monad m, DeltaParsing m) => m (Term Span) -> m (Term Span)
parseTmLoc p = do
  (t :~ l) <- spanned p
  return (review _TmLoc (l, t))

parseTerm :: (Monad m, DeltaParsing m) => m (Term Span)
parseTerm = parseTermF parseTmLoc

parseTermString :: MonadError Doc m => String -> m (Term Span)
parseTermString s = case parseString parseTerm (Lines 0 0 0 0) s of
  Success r -> return r
  Failure d -> throwError d
