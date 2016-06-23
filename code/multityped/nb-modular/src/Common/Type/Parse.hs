{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Type.Parse (
    ParseTypeInput(..)
  , HasParseTypeInput(..)
  , ParseTypeOutput(..)
  , HasParseTypeOutput(..)
  , mkParseType
  ) where

import Control.Lens.TH (makeClassy)

import Text.Trifecta.Delta (Delta(..))
import Text.Trifecta.Result (Result(..))
import Text.Trifecta.Parser (Parser, parseString)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import Common.Recursion

data ParseTypeInput ty =
  ParseTypeInput {
    _parseTypeSteps :: [Step (Parser ty)]
  }

makeClassy ''ParseTypeInput

instance Monoid (ParseTypeInput ty) where
  mempty =
    ParseTypeInput mempty
  mappend (ParseTypeInput p1) (ParseTypeInput p2) =
    ParseTypeInput (mappend p1 p2)

data ParseTypeOutput ty =
  ParseTypeOutput {
    _parseType :: Parser ty
  , _parseTypeString :: String -> Either Doc ty
  }

makeClassy ''ParseTypeOutput

mkParseType :: ParseTypeInput ty
            -> ParseTypeOutput ty
mkParseType (ParseTypeInput ps) =
  let
    p = combineSteps ps
  in
    ParseTypeOutput p (mkParseTypeString p)

mkParseTypeString :: Parser ty
                  -> String
                  -> Either Doc ty
mkParseTypeString p s =
  case parseString p (Lines 0 0 0 0) s of
    Success r -> Right r
    Failure d -> Left d
