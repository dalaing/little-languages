module Term.Parse where

import Control.Applicative ((<|>))
import Control.Lens (review)
import Data.String (IsString)

import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Expression
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS
import Text.Trifecta
import Text.Trifecta.Delta
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Term

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "i" lower alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["\\", "->", "@"]

identifier :: (IsString a, Monad m, TokenParsing m) => m a
identifier = ident style

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseVar :: (IsString a, Monad m, TokenParsing m) => m (Term a a)
parseVar = TmVar <$> identifier <?> "var"

parseApp :: (Monad m, TokenParsing m) => m (Term n a -> Term n a -> Term n a)
parseApp = TmApp <$ reserved "@"

parseLam :: (Eq a, IsString a, Monad m, TokenParsing m) => m (Term a a) -> m (Term a a)
parseLam p = curry (review _lam) <$ reserved "\\" <*> identifier <* reserved "->" <*> p

parseExpr :: (Eq a, IsString a, Monad m, TokenParsing m) => m (Term a a)
parseExpr = buildExpressionParser table parseTerm
  where
    table = [[Infix parseApp AssocRight]]

parseTerm :: (Eq a, IsString a, Monad m, TokenParsing m) => m (Term a a)
parseTerm =
  parens parseExpr <|>
  parseVar <|>
  parseLam parseExpr

parseTermString :: (Eq a, IsString a) => String -> Either Doc (Term a a)
parseTermString s = case parseString parseExpr (Lines 0 0 0 0) s of
  Success r -> Right r
  Failure d -> Left d

