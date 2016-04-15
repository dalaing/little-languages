{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Parsers for types of the B language.
-}
module Type.Parse (
    parseTypeRules
  , parseType
  ) where

import           Data.Foldable               (asum)

import qualified Data.HashSet                as HS
import           Text.Parser.Char            (alphaNum, upper)
import           Text.Parser.Combinators     ((<?>))
import           Text.Parser.Token           (IdentifierStyle (..),
                                              TokenParsing, reserve)
import           Text.Parser.Token.Highlight (Highlight (..))

import           Type                        (Type (..))

-- |
reservedTypes :: [String]
reservedTypes =
  ["Bool"]

-- |
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

-- |
reservedType :: ( Monad m
                , TokenParsing m
                )
             => String           -- ^
             -> m ()             -- ^
reservedType =
  reserve typeStyle

-- |
parseTyBool :: ( Monad m
               , TokenParsing m
               )
            => m Type           -- ^
parseTyBool =
  TyBool <$ reservedType "Bool"
  <?> "Bool"

-- |
parseTypeRules :: ( Monad m
                  , TokenParsing m
                  )
               => [m Type]         -- ^
parseTypeRules =
  [parseTyBool]

-- |
parseType :: ( Monad m
             , TokenParsing m
             )
          => m Type           -- ^
parseType =
  ( asum $
    parseTypeRules
  ) <?> "type"
