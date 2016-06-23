module Type.Parse where

import Control.Applicative ((<|>))
import Control.Lens (review)

import Text.Parser.Char
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS
import qualified Text.Trifecta as T
import Text.Trifecta.Delta
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Type

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "lb" upper alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["Bool", "->"]

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseTyBool :: (Monad m, TokenParsing m) => m (Type l)
parseTyBool = review _TyBool () <$ reserved "Bool"

parseTyArr :: (Monad m, TokenParsing m) => m (Type l -> Type l -> Type l)
parseTyArr = curry (review _TyArr) <$ reserved "->"

parseType :: (Monad m, TokenParsing m) => m (Type l)
parseType = buildExpressionParser table parseBase
  where
    table = [[Infix parseTyArr AssocRight]]

parseBase :: (Monad m, TokenParsing m) => m (Type l)
parseBase = parens parseType <|> parseTyBool

parseString :: String -> Either Doc (Type l)
parseString s = case T.parseString parseType (Lines 0 0 0 0) s of
  T.Success r -> Right r
  T.Failure d -> Left d
