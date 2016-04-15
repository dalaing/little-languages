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
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Type.Parse (
    ParseTypeRule(..)
  , ParseTypeInput(..)
  , ParseTypeOutput(..)
  , HasParseTypeOutput(..)
  , mkParseType
  ) where

import Control.Applicative ((<|>))
import Control.Monad (MonadPlus)
import Data.Foldable (asum)

import Control.Lens.TH (makeClassy)
import Text.Parser.Token (TokenParsing, parens)
import Text.Trifecta.Parser (Parser)

import Common.Parse (ReservedWords, GetReservedWords(..), ParserHelperOutput)

-- |
data ParseTypeRule ty =
    ParseTypeBase
      ReservedWords (ParserHelperOutput -> Parser ty)              -- ^
  | ParseTypeRecurse
      ReservedWords
      (ParserHelperOutput -> Parser ty -> Parser ty) -- ^

instance GetReservedWords (ParseTypeRule ty) where
  reservedWords (ParseTypeBase r _) = r
  reservedWords (ParseTypeRecurse r _) = r

-- |
fixParseTypeRule :: ParserHelperOutput
                 -> Parser ty
                 -> ParseTypeRule ty
                 -> Parser ty
fixParseTypeRule h _ (ParseTypeBase _ x) =
  x h
fixParseTypeRule h step (ParseTypeRecurse _ x) =
  x h step

-- |
data ParseTypeInput ty =
  ParseTypeInput [ParseTypeRule ty] -- ^

instance GetReservedWords (ParseTypeInput ty) where
  reservedWords (ParseTypeInput i) =
    foldMap reservedWords i

instance Monoid (ParseTypeInput ty) where
  mempty =
    ParseTypeInput mempty
  mappend (ParseTypeInput v1) (ParseTypeInput v2) =
    ParseTypeInput (mappend v1 v2)

-- |
data ParseTypeOutput ty =
  ParseTypeOutput {
    _parseType      :: Parser ty        -- ^
  , _parseTypeRules :: [Parser ty]      -- ^
  , _typeReservedWords :: ReservedWords -- ^
  }

makeClassy ''ParseTypeOutput

-- |
withParens :: ( MonadPlus m
              , TokenParsing m
              )
           => m ty         -- ^
           -> m ty         -- ^
withParens p =
  parens p <|>
  p

-- |
mkParseType :: ParserHelperOutput -- ^
            -> ParseTypeInput ty  -- ^
            -> ParseTypeOutput ty -- ^
mkParseType h p@(ParseTypeInput i) =
  let
    parseTypeRules' =
      fmap (fixParseTypeRule h parseType') i
    parseType' =
      withParens .
      asum $
      parseTypeRules'
  in
    ParseTypeOutput
      parseType'
      parseTypeRules'
      (reservedWords p)
