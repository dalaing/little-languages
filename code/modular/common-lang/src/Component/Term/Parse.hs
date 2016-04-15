{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Term.Parse (
    ParseTermRule(..)
  , ParseTermInput(..)
  , ParseTermOutput(..)
  , HasParseTermOutput(..)
  , mkParseTerm
  , withSpan
  ) where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus)
import Data.Foldable (asum)

import Control.Lens.TH (makeClassy)
import Control.Lens (review)
import Text.Parser.Token (TokenParsing, parens)
import Text.Trifecta.Parser (Parser)
import Text.Trifecta.Combinators (DeltaParsing, spanned)
import Text.Trifecta.Rendering (Span, Spanned(..))

import Common.Parse (ReservedWords, GetReservedWords(..), ParserHelperOutput)
import Component.Term.Note (AsNoteTerm(..), WithNoteTerm)

-- |
data ParseTermRule tm a =
    ParseTermBase
      ReservedWords (ParserHelperOutput -> Parser (tm a))              -- ^
  | ParseTermRecurse
      ReservedWords
      (ParserHelperOutput -> Parser (tm a) -> Parser (tm a)) -- ^

instance GetReservedWords (ParseTermRule tm a) where
  reservedWords (ParseTermBase r _) = r
  reservedWords (ParseTermRecurse r _) = r

-- |
fixParseTermRule :: ParserHelperOutput
                 -> Parser (tm a)
                 -> ParseTermRule tm a
                 -> Parser (tm a)
fixParseTermRule h _ (ParseTermBase _ x) =
  x h
fixParseTermRule h step (ParseTermRecurse _ x) =
  x h step

-- |
data ParseTermInput tm a =
  ParseTermInput [ParseTermRule tm a] -- ^

instance GetReservedWords (ParseTermInput tm a) where
  reservedWords (ParseTermInput i) =
    foldMap reservedWords i

instance Monoid (ParseTermInput tm a) where
  mempty =
    ParseTermInput mempty
  mappend (ParseTermInput v1) (ParseTermInput v2) =
    ParseTermInput (mappend v1 v2)

-- |
data ParseTermOutput tm a =
  ParseTermOutput {
    _parseTerm      :: (Parser (tm a) -> Parser (tm a)) -> Parser (tm a)        -- ^
  , _parseTermRules :: (Parser (tm a) -> Parser (tm a)) -> [Parser (tm a)]      -- ^
  , _termReservedWords :: ReservedWords -- ^
  }

makeClassy ''ParseTermOutput

-- |
withParens :: ( MonadPlus m
              , TokenParsing m
              )
           => m (tm a)         -- ^
           -> m (tm a)         -- ^
withParens p =
  parens p <|>
  p

withSpan :: ( Monad m
            , DeltaParsing m
            , WithNoteTerm tm Span a
            )
         => m (tm a)
         -> m (tm a)
withSpan p = do
  (tm :~ s) <- spanned p
  return $ review _TmNote (s, tm)

-- |
mkParseTerm :: ParserHelperOutput -- ^
            -> ParseTermInput tm a -- ^
            -> ParseTermOutput tm a -- ^
mkParseTerm h p@(ParseTermInput i) =
  let
    parseTermRules' pt =
      fmap (fixParseTermRule h (parseTerm' pt)) i
    parseTerm' pt =
      pt $
      withParens .
      asum .
      parseTermRules' $
      pt
  in
    ParseTermOutput
      parseTerm'
      parseTermRules'
      (reservedWords p)
