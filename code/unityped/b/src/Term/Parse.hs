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
style = IdentifierStyle "b" lower alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["true", "false", "if", "then", "else"]

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseTrue :: (Monad m, TokenParsing m) => m Term
parseTrue = TmTrue <$ reserved "true" <?> "true"

parseFalse :: (Monad m, TokenParsing m) => m Term
parseFalse = TmFalse <$ reserved "false" <?> "false"

parseIf :: (Monad m, TokenParsing m) => m Term -> m Term
parseIf p = TmIf <$ reserved "if" <*> p <* reserved "then" <*> p <* reserved "else" <*> p <?> "if-then-else"

parseTerm :: (Monad m, TokenParsing m) => m Term
parseTerm = asum [
    parseTrue
  , parseFalse
  , parseIf parseTerm
  ]

parseTermString :: String -> Either Doc Term
parseTermString s = case parseString parseTerm (Lines 0 0 0 0) s of
  Success r -> Right r
  Failure d -> Left d

