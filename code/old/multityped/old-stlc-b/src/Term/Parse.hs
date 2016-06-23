{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Term.Parse where

import Control.Lens (review)
import Data.Foldable (asum)
import Data.String (IsString)

import Control.Monad.Except

import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS
import Text.Trifecta
import Text.Trifecta.Delta
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Type
import Term

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "stlc-b" lower alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["true", "false", "if", "then", "else", "\\", "->", "@", ":"]

identifier :: (IsString a, Monad m, TokenParsing m)
           => m a
identifier = ident style

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseTrue :: (AsTermF f l n g a, Monad m, TokenParsing m) => m f
parseTrue = review _TmTrue () <$ reserved "true" <?> "true"

parseFalse :: (AsTermF f l n g a, Monad m, TokenParsing m) => m f
parseFalse = review _TmFalse () <$ reserved "false" <?> "false"

parseIf :: (AsTermF f l n g a, Monad m, TokenParsing m) => m (g a) -> m f
parseIf p = fmap (review _TmIf) ((,,) <$ reserved "if" <*> p <* reserved "then" <*> p <* reserved "else" <*> p <?> "if-then-else")

parseVar :: (AsTermF f l n g a, IsString a, Monad m, TokenParsing m)
         => m f
parseVar = review _TmVar <$> identifier <?> "var"

parseLam :: (AsTermF f l a g a, Eq a, IsString a, Monad m, TokenParsing m)
         => m (Type l)
         -> m (g a)
         -> m f
parseLam pt pe =
  curry (review _lam)
    <$ reserved "\\"
    <*> identifier
    <* reserved ":"
    <*> pt
    <* reserved "<-"
    <*> pe
    <?> "lam"

-- parseApp :: (AsTermF f l n g a, Monad m, TokenParsing m) => m (f -> f -> f)
-- parseApp = curry (review _TmApp) <$ reserved "@"

parseTermF :: (AsTermF f l n g a, Monad m, DeltaParsing m) => (m f -> m (g a)) -> m f
parseTermF p = asum [
      parseTrue
    , parseFalse
    , parseIf child
    ]
  where
    child = p (parseTermF p)

parseTmLoc :: (Monad m, DeltaParsing m) => m (Term Span n a) -> m (Term Span n a)
parseTmLoc p = do
  (t :~ l) <- spanned p
  return (review _TmLoc (l, t))

parseTerm :: (Monad m, DeltaParsing m) => m (Term Span n a)
parseTerm = parseTermF parseTmLoc

parseLoc' :: (Monad m, DeltaParsing m) => m (LocTerm Span n a) -> m (Loc Span n a)
parseLoc' p = do
  (t :~ l) <- spanned p
  return $ Loc l t

parseLocTerm :: (Monad m, DeltaParsing m) => m (LocTerm Span n a)
parseLocTerm = parseTermF parseLoc'

parseLoc :: (Monad m, DeltaParsing m) => m (Loc Span n a)
parseLoc = parseLoc' parseLocTerm

parseTermString :: MonadError Doc m => String -> m (Loc Span n a)
parseTermString s = case parseString parseLoc (Lines 0 0 0 0) s of
  Success r -> return r
  Failure d -> throwError d
