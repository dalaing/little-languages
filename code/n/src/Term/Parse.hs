{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Parsers for terms of the N language.
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
-- >>> let parse p s = case parseString p (Columns 0 0) s of Failure d -> Failure (plain d); Success s -> Success s

-- | The tokenizer style for identifiers in the N language.
--
-- N doesn't have identifiers, but we're going to be iterating towards a language that has them and
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
      ["pred"]

-- | A helper function to parse reserved identifiers.
reservedIdentifier :: ( Monad m
                      , TokenParsing m
                      )
                   => String
                   -> m ()
reservedIdentifier =
  reserve identifierStyle

-- | The tokenizer style for constructors in the N language.
--
-- N only has TmZero and TmSucc as constructors, but we're going to be iterating towards a
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
      [ "O"
      , "S"
      ]

-- | A helper function to parse a reserved constructor.
reservedConstructor :: ( Monad m
                       , TokenParsing m
                       )
                    => String
                    -> m ()
reservedConstructor =
  reserve constructorStyle

-- | A parser for 'TmZero'.
--
-- >>> parse parseTmZero "O"
-- Success TmZero
--
-- >>> parse parseTmZero "0"
-- Failure (interactive):1:1: error: expected: O
-- 0<EOF>
-- ^
--
-- >>> parse parseTmZero "potato"
-- Failure (interactive):1:1: error: expected: O
-- potato<EOF>
-- ^
parseTmZero :: ( Monad m
               , TokenParsing m
               )
            => m Term
parseTmZero =
  TmZero <$ reservedConstructor "O"
  <?> "O"

-- | A parser for 'TmSucc'.
--
-- >>> parse (parseTmSucc parseTerm) "S O"
-- Success (TmSucc TmZero)
--
-- >>> parse (parseTmSucc parseTerm) "s"
-- Failure (interactive):1:1: error: expected: S
-- s<EOF>
-- ^
--
-- >>> parse (parseTmSucc parseTerm) "potato"
-- Failure (interactive):1:1: error: expected: S
-- potato<EOF>
-- ^
parseTmSucc :: ( Monad m
               , TokenParsing m
               )
            => m Term -- ^ The term parser for N.
            -> m Term
parseTmSucc parse =
  TmSucc <$ reservedConstructor "S" <*> parse
  <?> "S"

-- | A parser for 'TmPred'.
--
-- >>> parse (parseTmPred parseTerm) "pred O"
-- Success (TmPred TmZero)
--
-- >>> parse (parseTmPred parseTerm) "Pred O"
-- Failure (interactive):1:1: error: expected: pred
-- Pred O<EOF>
-- ^
--
-- >>> parse (parseTmPred parseTerm) "potato"
-- Failure (interactive):1:1: error: expected: pred
-- potato<EOF>
-- ^
parseTmPred :: ( Monad m
               , TokenParsing m
               )
            => m Term -- ^ The term parser for N.
            -> m Term
parseTmPred parse =
  TmPred <$ reservedIdentifier "pred" <*> parse
  <?> "pred"


-- | The set of parsing rules for terms of the N language.

-- The order shouldn't matter that much because we're
-- using a token parser and we reserved our keywords.
parseTermRules :: ( Monad m
                  , TokenParsing m
                  )
               => [m Term]
parseTermRules =
  [ parseTmZero
  , parseTmSucc parseTerm
  , parseTmPred parseTerm
  ]

-- | The parser for terms of the N language.
--
-- This function is built from the contents of 'parseTermRules',
-- with added support for parentheses.
--
-- >>> parse parseTerm "S S O"
-- Success (TmSucc (TmSucc TmZero))
--
-- >>> parse parseTerm "(S ((S (O))))"
-- Success (TmSucc (TmSucc TmZero))
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

