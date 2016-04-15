{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Common.Parse (
    parseFromString
  , ReservedWords(..)
  , GetReservedWords(..)
  , reserveIdentifiers
  , reserveConstructors
  , reserveOperators
  , ParserHelperInput(..)
  , ParserHelperOutput(..)
  , HasParserHelperOutput(..)
  , defaultTermParserHelper
  , defaultTypeParserHelper
  , mkParserHelper
  ) where

import           Control.Applicative          ((<|>))

import           Control.Lens                 (over)
import           Control.Lens.TH              (makeClassy)
import qualified Data.HashSet                 as HS (fromList, union)
import           Text.Parser.Char             (alphaNum, char, lower, oneOf,
                                               upper)
import           Text.Parser.Token            (IdentifierStyle (..), ident,
                                               reserve, styleReserved)
import           Text.Parser.Token.Highlight  (Highlight (..))
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta.Delta          (Delta (..))
import           Text.Trifecta.Parser         (Parser, parseString)
import           Text.Trifecta.Result         (Result (..))

-- |
parseFromString :: Parser a     -- ^
                -> String       -- ^
                -> Either Doc a -- ^
parseFromString p s =
  case parseString p (Lines 0 0 0 0) s of
    Success r -> Right r
    Failure d -> Left d

data ParserHelperInput =
  ParserHelperInput {
    _identifierStyle  :: IdentifierStyle Parser
  , _constructorStyle :: IdentifierStyle Parser
  , _operatorStyle    :: IdentifierStyle Parser
  }

defaultTermParserHelper :: ParserHelperInput
defaultTermParserHelper =
    ParserHelperInput i c o
  where
    i = IdentifierStyle
      { _styleName     = "identifier"
      , _styleStart    = lower
      , _styleLetter   = alphaNum <|> char '_'
      , _styleReserved = mempty
      , _styleHighlight = Identifier
      , _styleReservedHighlight = ReservedIdentifier
      }
    c = IdentifierStyle
      { _styleName     = "constructor"
      , _styleStart    = upper
      , _styleLetter   = alphaNum <|> char '_'
      , _styleReserved = mempty
      , _styleHighlight = Constructor
      , _styleReservedHighlight = ReservedConstructor
      }
    o = IdentifierStyle
      { _styleName     = "operator"
      , _styleStart    = _styleLetter o
      , _styleLetter   = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , _styleReserved = mempty
      , _styleHighlight = Operator
      , _styleReservedHighlight = ReservedOperator
      }

defaultTypeParserHelper :: ParserHelperInput
defaultTypeParserHelper =
    ParserHelperInput i c o
  where
    i = IdentifierStyle
      { _styleName     = "identifier"
      , _styleStart    = upper
      , _styleLetter   = alphaNum <|> char '_'
      , _styleReserved = mempty
      , _styleHighlight = Identifier
      , _styleReservedHighlight = ReservedIdentifier
      }
    c = IdentifierStyle
      { _styleName     = "constructor"
      , _styleStart    = upper
      , _styleLetter   = alphaNum <|> char '_'
      , _styleReserved = mempty
      , _styleHighlight = Constructor
      , _styleReservedHighlight = ReservedConstructor
      }
    o = IdentifierStyle
      { _styleName     = "operator"
      , _styleStart    = _styleLetter o
      , _styleLetter   = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , _styleReserved = mempty
      , _styleHighlight = Operator
      , _styleReservedHighlight = ReservedOperator
      }

data ReservedWords =
  ReservedWords {
    _reservedIdentifiers  :: [String]
  , _reservedConstructors :: [String]
  , _reservedOperators    :: [String]
  }

class GetReservedWords r where
  reservedWords :: r -> ReservedWords

instance GetReservedWords ReservedWords where
  reservedWords = id

reserveIdentifiers :: [String]
                   -> ReservedWords
reserveIdentifiers ss =
  ReservedWords ss mempty mempty

reserveConstructors :: [String]
                    -> ReservedWords
reserveConstructors ss =
  ReservedWords mempty ss mempty

reserveOperators :: [String]
                 -> ReservedWords
reserveOperators =
  ReservedWords mempty mempty

instance Monoid ReservedWords where
  mempty =
    ReservedWords mempty mempty mempty
  mappend (ReservedWords i1 c1 o1) (ReservedWords i2 c2 o2) =
    ReservedWords (mappend i1 i2) (mappend c1 c2) (mappend o1 o2)

data ParserHelperOutput =
  ParserHelperOutput {
    _identifier          :: Parser String
  , _reservedIdentifier  :: String -> Parser ()
  , _constructor         :: Parser String
  , _reservedConstructor :: String -> Parser ()
  , _operator            :: Parser String
  , _reservedOperator    :: String -> Parser ()
  }

makeClassy ''ParserHelperOutput

mkParserHelper :: ReservedWords
               -> ParserHelperInput
               -> ParserHelperOutput
mkParserHelper (ReservedWords ri rc ro) (ParserHelperInput si sc so) =
    ParserHelperOutput
      (ident si') (reserve si')
      (ident sc') (reserve sc')
      (ident so') (reserve so')
  where
    addWords ss = HS.union (HS.fromList ss)
    add w = over styleReserved (addWords w)
    si' = add ri si
    sc' = add rc sc
    so' = add ro so
