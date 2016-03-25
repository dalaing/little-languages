module Term.Parse where

import Control.Applicative ((<|>))

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
    res = HS.fromList ["+", "-", "*", "^"]

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseInt :: (Monad m, TokenParsing m) => m Term
parseInt = (TmInt . fromInteger) <$> integer <?> "int"

parseAdd :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseAdd = TmAdd <$ reserved "+"

parseSub :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseSub = TmSub <$ reserved "-"

parseMul :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseMul = TmMul <$ reserved "*"

parseExp :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseExp = TmExp <$ reserved "^"

parseExpr :: (Monad m, TokenParsing m) => m Term
parseExpr = buildExpressionParser table parseTerm
  where
    table =
      [ [Infix parseAdd AssocLeft, Infix parseSub AssocLeft]
      , [Infix parseMul AssocLeft]
      , [Infix parseExp AssocRight]
      ]

parseTerm :: (Monad m, TokenParsing m) => m Term
parseTerm = parens parseExpr <|> parseInt

parseTermString :: String -> Either Doc Term
parseTermString s = case parseString parseExpr (Lines 0 0 0 0) s of
  Success r -> Right r
  Failure d -> Left d

