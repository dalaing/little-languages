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
style = IdentifierStyle "n" lower alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["O", "S", "pred"]

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseZero :: (Monad m, TokenParsing m) => m Term
parseZero = TmZero <$ reserved "O" <?> "O"

parseSucc :: (Monad m, TokenParsing m) => m Term -> m Term
parseSucc p = TmSucc <$ reserved "S" <*> p <?> "succ"

parsePred :: (Monad m, TokenParsing m) => m Term -> m Term
parsePred p = TmPred <$ reserved "pred" <*> p <?> "pred"

parseTerm :: (Monad m, TokenParsing m) => m Term
parseTerm = asum [
    parseZero
  , parseSucc parseTerm
  , parsePred parseTerm
  ]

parseTermString :: String -> Either Doc Term
parseTermString s = case parseString parseTerm (Lines 0 0 0 0) s of
  Success r -> Right r
  Failure d -> Left d

