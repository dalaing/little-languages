{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Parsers for terms of the B language.
-}
module Term.Parse (
    withParens
  , parseTermRules
  , parseTerm
  ) where

import           Control.Applicative          ((<|>))
import           Data.Foldable                (asum)

import qualified Data.HashSet                 as HS
import           Text.Parser.Char             (alphaNum, char, lower, upper)
import           Text.Parser.Combinators      ((<?>))
import           Text.Parser.Token            (IdentifierStyle (..),
                                               TokenParsing, parens, reserve)
import           Text.Parser.Token.Highlight  (Highlight (..))

import           Term                         (Term (..))

-- |
reservedIdentifiers :: [String]
reservedIdentifiers =
  [ "if"
  , "then"
  , "else"
  ]

-- |
identifierStyle :: TokenParsing m
                => IdentifierStyle m
identifierStyle =
  IdentifierStyle {
    _styleName              = "identitfier"
  , _styleStart             = lower <|> char '_'
  , _styleLetter            = alphaNum <|> char '_'
  , _styleReserved          = HS.fromList reservedIdentifiers
  , _styleHighlight         = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

-- |
reservedIdentifier :: ( Monad m
                      , TokenParsing m
                      )
                   => String            -- ^
                   -> m ()              -- ^
reservedIdentifier =
  reserve identifierStyle

-- |
reservedConstructors :: [String]
reservedConstructors =
  [ "False"
  , "True"
  ]

-- |
constructorStyle :: TokenParsing m
                 => IdentifierStyle m
constructorStyle =
  IdentifierStyle {
    _styleName              = "constructor"
  , _styleStart             = upper
  , _styleLetter            = alphaNum <|> char '_'
  , _styleReserved          = HS.fromList reservedConstructors
  , _styleHighlight         = Constructor
  , _styleReservedHighlight = ReservedConstructor
  }

-- |
reservedConstructor :: ( Monad m
                       , TokenParsing m
                       )
                    => String           -- ^
                    -> m ()             -- ^
reservedConstructor =
  reserve constructorStyle

-- |
parseTmFalse :: ( Monad m
                , TokenParsing m
                )
             => m Term           -- ^
parseTmFalse =
  TmFalse <$ reservedConstructor "False"
  <?> "False"

-- |
parseTmTrue :: ( Monad m
               , TokenParsing m
               )
            => m Term           -- ^
parseTmTrue =
  TmTrue <$ reservedConstructor "True"
  <?> "True"

-- |
parseTmIf :: ( Monad m
             , TokenParsing m
             )
          => m Term           -- ^
          -> m Term           -- ^
parseTmIf parseTerm =
  TmIf <$
    reservedIdentifier "if" <*> parseTerm <*
    reservedIdentifier "then" <*> parseTerm <*
    reservedIdentifier "else" <*> parseTerm
  <?> "if-then-else"

-- |
parseTermRules :: ( Monad m
                  , TokenParsing m
                  )
               => [m Term]         -- ^
parseTermRules =
  [ parseTmFalse
  , parseTmTrue
  , parseTmIf parseTerm
  ]

-- |
withParens :: TokenParsing m
           => m Term         -- ^
           -> m Term         -- ^
withParens p =
  parens p <|>
  p

-- |
parseTerm :: ( Monad m
             , TokenParsing m
             )
          => m Term           -- ^
parseTerm =
  ( withParens .
    asum $
    parseTermRules
  ) <?> "term"

