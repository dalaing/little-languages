module Term.Parser (
    termParser
  ) where

import Control.Applicative
import Data.String (IsString)
import Data.Functor

import qualified Data.HashSet as HS

import qualified Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Trifecta

import Term

identStyle :: CharParsing m => IdentifierStyle m
identStyle = IdentifierStyle "" lower alphaNum (HS.fromList ["lam"]) Identifier ReservedIdentifier

identifier :: (TokenParsing m, Monad m, IsString s) => m s
identifier = ident identStyle 

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = reserve identStyle

-- TODO extensibility in the face of needing locations or not
-- TODO something like Name for binding types, but also capturing the
-- locations of the various pieces of the bindings

termParser :: (DeltaParsing m, Monad m) => m (Term Span String String)
termParser = do
    (t :~ s) <- spanned termParser'
    return (Loc s t)

termParser' :: (DeltaParsing m, Monad m) => m (Term Span String String)
termParser' = 
      try varParser 
  <|> parens (try lamParser <|> appParser)
  <?> "term"

varNameParser :: (DeltaParsing m, Monad m) => m String
varNameParser = identifier

varParser :: (DeltaParsing m, Monad m) => m (Term Span String String)
varParser = Var <$> varNameParser <?> "var"

appParser :: (DeltaParsing m, Monad m) => m (Term Span String String)
appParser = App <$> termParser <*> termParser <?> "app"

lamParser :: (DeltaParsing m, Monad m) => m (Term Span String String)
lamParser = lam_ <$> (reserved "lam" *> varNameParser) <*> termParser <?> "lam"
