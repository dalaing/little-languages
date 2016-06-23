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

style :: CharParsing m
      => IdentifierStyle m
style =
    IdentifierStyle "stlc-b" lower alphaNum res Identifier ReservedIdentifier
  where
    res = HS.fromList ["true", "false", "if", "then", "else"]

reserved :: (Monad m, TokenParsing m)
         => String
         -> m ()
reserved =
  reserve style

parseTrue :: ( Monad m
             , TokenParsing m)
          => m (Term n l a)
parseTrue =
  review _TmTrue () <$ reserved "true" <?> "true"

parseFalse :: ( Monad m
              , TokenParsing m)
           => m (Term n l a)
parseFalse =
  review _TmFalse () <$ reserved "false" <?> "false"

parseIf :: ( Monad m
           , TokenParsing m)
        => m (Term n l a)
        -> m (Term n l a)
parseIf p =
  fmap (review _TmIf) ((,,) <$ reserved "if" <*> p <* reserved "then" <*> p <* reserved "else" <*> p <?> "if-then-else")

parseTermF :: (Monad m, DeltaParsing m)
           => (m (Term n l a) -> m (Term n l a))
           -> m (Term n l a)
parseTermF p = p . asum $ [
      parseTrue
    , parseFalse
    , parseIf child
    ]
  where
    child = parseTermF p

parseTmLoc :: ( Monad m
              , DeltaParsing m)
           => m (Term n Span a)
           -> m (Term n Span a)
parseTmLoc p = do
  (t :~ l) <- spanned p
  return (review _TmLoc (l, t))

parseTerm :: ( Monad m
             , DeltaParsing m
             )
           => m (Term n Span a)
parseTerm =
  parseTermF parseTmLoc

parseTermString :: MonadError Doc m
                => String
                -> m (Term n Span a)
parseTermString s =
  case parseString parseTerm (Lines 0 0 0 0) s of
    Success r -> return r
    Failure d -> throwError d
