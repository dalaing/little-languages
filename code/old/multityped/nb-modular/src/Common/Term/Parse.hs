{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Term.Parse (
    ParseTermInput(..)
  , HasParseTermInput(..)
  , ParseTermOutput(..)
  , HasParseTermOutput(..)
  , mkParseTerm
  ) where

import Control.Lens.TH (makeClassy)

import Text.Trifecta.Delta (Delta(..))
import Text.Trifecta.Result (Result(..))
import Text.Trifecta.Parser (Parser, parseString)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Common.Recursion

data ParseTermInput tm =
  ParseTermInput {
    _parseTermSteps :: [Step (Parser tm)]
  }

makeClassy ''ParseTermInput

instance Monoid (ParseTermInput tm) where
  mempty = ParseTermInput mempty
  mappend (ParseTermInput p1) (ParseTermInput p2) =
    ParseTermInput (mappend p1 p2)

data ParseTermOutput tm =
  ParseTermOutput {
    _parseTerm :: Parser tm
  , _parseTermString :: String -> Either Doc tm
  }

makeClassy ''ParseTermOutput

mkParseTerm :: ParseTermInput tm
            -> ParseTermOutput tm
mkParseTerm (ParseTermInput ps) =
  let
    p = combineSteps ps
  in
    ParseTermOutput p (mkParseTermString p)

mkParseTermString :: Parser tm
                  -> String
                  -> Either Doc tm
mkParseTermString p s =
  case parseString p (Lines 0 0 0 0) s of
    Success r -> Right r
    Failure d -> Left d
