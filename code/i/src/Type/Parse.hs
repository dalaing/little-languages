{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Parsers for types of the I language.
-}
module Type.Parse (
    parseTypeRules
  , parseType
  ) where

-- from 'base'
import           Control.Applicative         ((<|>))
import           Data.Foldable               (asum)

-- from 'unordered-containers'
import qualified Data.HashSet                as HS (fromList)

-- from 'parsers'
import           Text.Parser.Char            (alphaNum, upper)
import           Text.Parser.Combinators     ((<?>))
import           Text.Parser.Token           (IdentifierStyle (..),
                                              TokenParsing, parens, reserve)
import           Text.Parser.Token.Highlight (Highlight (..))

-- local
import           Type                        (Type (..))

-- $setup
-- >>> import Text.Trifecta.Parser
-- >>> import Text.Trifecta.Result
-- >>> import Text.Trifecta.Delta
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let parse p s = case parseString p (Lines 0 0 0 0) s of Failure d -> Failure (plain d); Success s -> Success s

-- | The tokenizer style for types in the B language.
typeStyle :: TokenParsing m
          => IdentifierStyle m
typeStyle =
    IdentifierStyle {
      _styleName              = "type"
    , _styleStart             = upper
    , _styleLetter            = alphaNum
    , _styleReserved          = HS.fromList reservedTypes
    , _styleHighlight         = Constructor
    , _styleReservedHighlight = ReservedConstructor
    }
  where
    reservedTypes =
      ["Int"]

-- | A helper function to parse reserved types.
reservedType :: ( Monad m
                , TokenParsing m
                )
             => String
             -> m ()
reservedType =
  reserve typeStyle

-- | A parser for 'Int'.
--
-- >>> parse parseTyInt "Int"
-- Success TyInt
--
-- >>> parse parseTyInt "int"
-- Failure (interactive):1:1: error: expected: Int
-- int<EOF>
-- ^
--
-- >>> parse parseTyInt "potato"
-- Failure (interactive):1:1: error: expected: Int
-- potato<EOF>
-- ^
parseTyInt :: ( Monad m
              , TokenParsing m
              )
           => m Type
parseTyInt =
  TyInt <$ reservedType "Int"
  <?> "Int"

-- | The set of parsing rules for types of the I language.
parseTypeRules :: ( Monad m
                  , TokenParsing m
                  )
               => [m Type]
parseTypeRules =
  [parseTyInt]

-- | The parser for types of the I languge.
--
-- This function is built from the contents of 'parseTypeRules',
-- with added support for parentheses.
--
-- >>> parse parseType "Int"
-- Success TyInt
--
-- >>> parse parseType "((Int))"
-- Success TyInt
--
-- >>> parse parseType "potato"
-- Failure (interactive):1:1: error: expected: type
-- potato<EOF>
-- ^
--
-- >>> parse parseType "((potato))"
-- Failure (interactive):1:3: error: expected: type
-- ((potato))<EOF>
--   ^
--
parseType :: ( Monad m
             , TokenParsing m
             )
          => m Type
parseType =
  (asum parseTypeRules <|> parens parseType)
  <?> "type"
