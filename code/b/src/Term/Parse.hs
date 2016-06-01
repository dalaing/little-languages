{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Parsers for terms of the B language.
-}
module Term.Parse (
    parseTermRules
  , parseTerm
  ) where

-- from 'base'
import           Control.Applicative         ((<|>))
import           Data.Foldable               (asum)

-- from 'unordered-containers'
import qualified Data.HashSet                as HS

-- from 'parsers'
import           Text.Parser.Char            (alphaNum, char, lower, upper)
import           Text.Parser.Combinators     ((<?>))
import           Text.Parser.Token           (IdentifierStyle (..),
                                              TokenParsing, parens, reserve)
import           Text.Parser.Token.Highlight (Highlight (..))

-- local
import           Term                        (Term (..))

-- $setup
-- >>> import Text.Trifecta.Parser
-- >>> import Text.Trifecta.Result
-- >>> import Text.Trifecta.Delta
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let parse p s = case parseString p (Lines 0 0 0 0) s of Failure d -> Failure (plain d); Success s -> Success s

-- | The tokenizer style for identifiers in the B language.
--
-- B doesn't have identifiers, but we're going to be iterating towards a language that has them and
-- we know what they're going to look like when they arrive, so we start setting them up now.
--
-- We include the reserved words that might clash with the identifier parser
-- in here, despite the fact that they're not actually identifiers.
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
  where
    reservedIdentifiers =
      [ "if"
      , "then"
      , "else"
      ]

-- | A helper function to parse reserved identifiers.
reservedIdentifier :: ( Monad m
                      , TokenParsing m
                      )
                   => String
                   -> m ()
reservedIdentifier =
  reserve identifierStyle

-- | The tokenizer style for constructors in the B language.
--
-- B only has TmFalse and TmTrue as constructors, but we're going to be iterating towards a
-- language that has user-defined constructors and we know what they're going to look like when they arrive, so we start setting them up now.
--
-- We include the reserved words that might clash with the constructor parser
-- in here. It is a better situation than with 'identifierStyle', since these reserved words are actually constructors.
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
  where
    reservedConstructors =
      [ "False"
      , "True"
      ]

-- | A helper function to parse a reserved constructor.
reservedConstructor :: ( Monad m
                       , TokenParsing m
                       )
                    => String
                    -> m ()
reservedConstructor =
  reserve constructorStyle

-- | A parser for 'TmFalse'.
--
-- >>> parse parseTmFalse "False"
-- Success TmFalse
--
-- >>> parse parseTmFalse "false"
-- Failure (interactive):1:1: error: expected: False
-- false<EOF>
-- ^
--
-- >>> parse parseTmFalse "potato"
-- Failure (interactive):1:1: error: expected: False
-- potato<EOF>
-- ^
parseTmFalse :: ( Monad m
                , TokenParsing m
                )
             => m Term
parseTmFalse =
  TmFalse <$ reservedConstructor "False"
  <?> "False"

-- | A parser for 'TmTrue'.
--
-- >>> parse parseTmTrue "True"
-- Success TmTrue
--
-- >>> parse parseTmTrue "true"
-- Failure (interactive):1:1: error: expected: True
-- true<EOF>
-- ^
--
-- >>> parse parseTmTrue "potato"
-- Failure (interactive):1:1: error: expected: True
-- potato<EOF>
-- ^
parseTmTrue :: ( Monad m
               , TokenParsing m
               )
            => m Term
parseTmTrue =
  TmTrue <$ reservedConstructor "True"
  <?> "True"

-- | A parser for 'TmIf'.
--
-- >>> parse (parseTmIf parseTerm) "if False then False else True"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse (parseTmIf parseTerm) "if False then False else"
-- Failure (interactive):1:25: error: unexpected
--     EOF, expected: end of "else",
--     term
-- if False then False else<EOF>
--                         ^
--
-- >>> parse (parseTmIf parseTerm) "if False then False"
-- Failure (interactive):1:20: error: unexpected
--     EOF, expected: "else",
--     end of "False"
-- if False then False<EOF>
--                    ^
--
-- >>> parse (parseTmIf parseTerm) "if potato then False else True"
-- Failure (interactive):1:4: error: expected: term
-- if potato then False else True<EOF>
--    ^
--
-- >>> parse (parseTmIf parseTerm) "if False potato False else True"
-- Failure (interactive):1:10: error: expected: "then"
-- if False potato False else True<EOF>
--          ^
parseTmIf :: ( Monad m
             , TokenParsing m
             )
          => m Term -- ^ The term parser for B.
          -> m Term
parseTmIf parse =
  TmIf <$
    reservedIdentifier "if" <*> parse <*
    reservedIdentifier "then" <*> parse <*
    reservedIdentifier "else" <*> parse
  <?> "if-then-else"

-- | The set of parsing rules for terms of the B language.

-- The order shouldn't matter that much because we're
-- using a token parser and we reserved our keywords.
parseTermRules :: ( Monad m
                  , TokenParsing m
                  )
               => [m Term]
parseTermRules =
  [ parseTmFalse
  , parseTmTrue
  , parseTmIf parseTerm
  ]

-- | The parser for terms of the B language.
--
-- This function is built from the contents of 'parseTermRules',
-- with added support for parentheses.
--
-- >>> parse parseTerm "if False then False else True"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTerm "((if (False) then ((False)) else (((True)))))"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTerm "potato"
-- Failure (interactive):1:1: error: expected: term
-- potato<EOF>
-- ^
--
-- >>> parse parseTerm "((potato))"
-- Failure (interactive):1:3: error: expected: term
-- ((potato))<EOF>
--   ^
--
parseTerm :: ( Monad m
             , TokenParsing m
             )
          => m Term
parseTerm =
  (asum parseTermRules <|> parens parseTerm)
  <?> "term"

