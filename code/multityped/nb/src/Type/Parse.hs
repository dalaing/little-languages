module Type.Parse where

import Control.Applicative ((<|>))

import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS
import Text.Trifecta
import Text.Trifecta.Delta
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Type

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "nb" upper alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["Bool", "Nat"]

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseTyBool :: (Monad m, TokenParsing m) => m Type
parseTyBool = TyBool <$ reserved "Bool" <?> "Bool"

parseTyNat :: (Monad m, TokenParsing m) => m Type
parseTyNat = TyNat <$ reserved "Nat" <?> "Nat"

parseType :: (Monad m, TokenParsing m) => m Type
parseType = parseTyBool <|> parseTyNat

parseTypeString :: String -> Either Doc Type
parseTypeString s = case parseString parseType (Lines 0 0 0 0) s of
  Success r -> Right r
  Failure d -> Left d
