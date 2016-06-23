module Term.Parse where

import Data.Foldable (asum)

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

parseZero :: (Monad m, TokenParsing m) => m Term
parseZero = TmZero <$ reserved "O" <?> "O"

parseSucc :: (Monad m, TokenParsing m) => m Term -> m Term
parseSucc p = TmSucc <$ reserved "S" <*> p <?> "succ"

parsePred :: (Monad m, TokenParsing m) => m Term -> m Term
parsePred p = TmPred <$ reserved "pred" <*> p <?> "pred"

parseTrue :: (Monad m, TokenParsing m) => m Term
parseTrue = TmTrue <$ reserved "true" <?> "true"

parseFalse :: (Monad m, TokenParsing m) => m Term
parseFalse = TmFalse <$ reserved "false" <?> "false"

parseIf :: (Monad m, TokenParsing m) => m Term -> m Term
parseIf p = TmIf <$ reserved "if" <*> p <* reserved "then" <*> p <* reserved "else" <*> p <?> "if-then-else"

parseIsZero :: (Monad m, TokenParsing m) => m Term -> m Term
parseIsZero p = TmIsZero <$ reserved "isZero" <*> p <?> "isZero"

parseTerm :: (Monad m, TokenParsing m) => m Term
parseTerm = asum [
    parseZero
  , parseSucc parseTerm
  , parsePred parseTerm
  , parseTrue
  , parseFalse
  , parseIf parseTerm
  , parseIsZero parseTerm
  ]

parseTermString :: String -> Either Doc Term
parseTermString s = case parseString parseTerm (Lines 0 0 0 0) s of
  Success r -> Right r
  Failure d -> Left d

