{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

An internal module used to compare the impact of using the various different kinds of parsers in the 'parsers' package.
-}

module Term.Parse.Zoo (
  ) where

-- from 'base'
import           Control.Applicative          ((<|>))
import           Data.Foldable                (asum)

-- from 'unordered-containers'
import qualified Data.HashSet                 as HS

-- from 'parsers'
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight

-- $setup
-- >>> import Text.Trifecta.Parser
-- >>> import Text.Trifecta.Result
-- >>> import Text.Trifecta.Delta
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let parse p s = case parseString p (Lines 0 0 0 0) s of Failure d -> Failure (plain d); Success s -> Success s

identifierStyle :: TokenParsing m
                => IdentifierStyle m
identifierStyle =
    IdentifierStyle {
      _styleName              = "identifier"
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

-- | A helper function to parse a reserved identifier.
reservedIdentifier :: ( Monad m
                      , TokenParsing m
                      )
                   => String
                   -> m ()
reservedIdentifier =
  reserve identifierStyle

identifier :: ( Monad m
              , TokenParsing m
              )
           => m String
identifier =
  ident identifierStyle

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

constructor :: ( Monad m
               , TokenParsing m
               )
            => m String
constructor =
  ident constructorStyle

data Term =
    TmVar String
  | TmFalse
  | TmTrue
  | TmIf Term Term Term
  deriving (Eq, Ord, Show)

parseTmVarChar :: (Monad m, CharParsing m) => m Term
parseTmVarChar =
  (\x y -> TmVar (x : y)) <$> lower <*> many (alphaNum <|> char '_')

parseTmVarCharName :: (Monad m, CharParsing m) => m Term
parseTmVarCharName =
  parseTmVarChar <?> "Var"

parseTmVarToken :: (Monad m, TokenParsing m) => m Term
parseTmVarToken =
  TmVar <$> identifier

parseTmVarTokenName :: (Monad m, TokenParsing m) => m Term
parseTmVarTokenName =
  parseTmVarToken <?> "Var"

parseTmFalseChar :: (Monad m, CharParsing m) => m Term
parseTmFalseChar =
  TmFalse <$ string "False"

parseTmFalseCharName :: (Monad m, CharParsing m) => m Term
parseTmFalseCharName =
  parseTmFalseChar <?> "False"

parseTmFalseToken :: (Monad m, TokenParsing m) => m Term
parseTmFalseToken =
  TmFalse <$ reservedConstructor "False"

parseTmFalseTokenName :: (Monad m, TokenParsing m) => m Term
parseTmFalseTokenName =
  parseTmFalseToken <?> "False"

parseTmTrueChar :: (Monad m, CharParsing m) => m Term
parseTmTrueChar =
  TmTrue <$ string "True"

parseTmTrueCharName :: (Monad m, CharParsing m) => m Term
parseTmTrueCharName =
  parseTmTrueChar <?> "True"

parseTmTrueToken :: (Monad m, TokenParsing m) => m Term
parseTmTrueToken =
  TmTrue <$ reservedConstructor "True"

parseTmTrueTokenName :: (Monad m, TokenParsing m) => m Term
parseTmTrueTokenName =
  parseTmTrueToken <?> "True"

parseTmIfChar :: (Monad m, CharParsing m) => m Term -> m Term
parseTmIfChar p =
  TmIf <$
    string "if"   <* spaces <*> p <* spaces <*
    string "then" <* spaces <*> p <* spaces <*
    string "else" <* spaces <*> p

parseTmIfCharName :: (Monad m, CharParsing m) => m Term -> m Term
parseTmIfCharName p =
  parseTmIfChar p <?> "if-then-else"

parseTmIfToken :: (Monad m, TokenParsing m) => m Term -> m Term
parseTmIfToken p =
  TmIf <$
    reservedIdentifier "if" <*> p <*
    reservedIdentifier "then" <*> p <*
    reservedIdentifier "else" <*> p

parseTmIfTokenName :: (Monad m, TokenParsing m) => m Term -> m Term
parseTmIfTokenName p =
  parseTmIfToken p <?> "if-then-else"

parseTermRulesCharOrder :: (Monad m, CharParsing m) => [m Term]
parseTermRulesCharOrder =
  [parseTmVarChar, parseTmFalseChar, parseTmTrueChar, parseTmIfChar parseTermCharOrder]

parseTermRulesChar :: (Monad m, CharParsing m) => [m Term]
parseTermRulesChar =
  [parseTmIfChar parseTermChar, parseTmVarChar, parseTmFalseChar, parseTmTrueChar]

parseTermRulesCharName :: (Monad m, CharParsing m) => [m Term]
parseTermRulesCharName =
  [parseTmIfCharName parseTermCharName, parseTmVarCharName, parseTmFalseCharName, parseTmTrueCharName]

parseTermRulesToken :: (Monad m, TokenParsing m) => [m Term]
parseTermRulesToken =
  [parseTmVarToken, parseTmFalseToken, parseTmTrueToken, parseTmIfToken parseTermToken]

parseTermRulesTokenName :: (Monad m, TokenParsing m) => [m Term]
parseTermRulesTokenName =
  [parseTmVarTokenName, parseTmFalseTokenName, parseTmTrueTokenName, parseTmIfTokenName parseTermTokenName]

-- |
-- >>> parse parseTermCharOrder "potato"
-- Success (TmVar "potato")
--
-- >>> parse parseTermCharOrder "Potato"
-- Failure (interactive):1:1: error: expected: "(",
--     "False", "True", "if",
--     lowercase letter
-- Potato<EOF>
-- ^
--
-- >>> parse parseTermCharOrder "False"
-- Success TmFalse
--
-- >>> parse parseTermCharOrder "false"
-- Success (TmVar "false")
--
-- >>> parse parseTermCharOrder "True"
-- Success TmTrue
--
-- >>> parse parseTermCharOrder "true"
-- Success (TmVar "true")
--
-- >>> parse parseTermCharOrder "(False ) "
-- Success TmFalse
--
-- >>> parse parseTermCharOrder "False ) "
-- Success TmFalse
--
-- >>> parse parseTermCharOrder "(False  "
-- Failure (interactive):1:9: error: unexpected
--     EOF, expected: ")", space
-- (False  <EOF>
--         ^
-- >>> parse parseTermCharOrder "if False then False else True"
-- Success (TmVar "if")
--
parseTermCharOrder :: (Monad m, CharParsing m) => m Term
parseTermCharOrder =
    asum parseTermRulesCharOrder <|> p parseTermCharOrder
  where
    p = between (char '(' <* spaces) (spaces <* char ')')

-- |
-- >>> parse parseTermChar "potato"
-- Success (TmVar "potato")
--
-- >>> parse parseTermChar "Potato"
-- Failure (interactive):1:1: error: expected: "(",
--     "False", "True", "if",
--     lowercase letter
-- Potato<EOF>
-- ^
--
-- >>> parse parseTermChar "False"
-- Success TmFalse
--
-- >>> parse parseTermChar "false"
-- Success (TmVar "false")
--
-- >>> parse parseTermChar "True"
-- Success TmTrue
--
-- >>> parse parseTermChar "true"
-- Success (TmVar "true")
--
-- >>> parse parseTermChar "(False ) "
-- Success TmFalse
--
-- >>> parse parseTermChar "False ) "
-- Success TmFalse
--
-- >>> parse parseTermChar "(False  "
-- Failure (interactive):1:9: error: unexpected
--     EOF, expected: ")", space
-- (False  <EOF>
--         ^
-- >>> parse parseTermChar "if False then False else True"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTermChar "if (False) then ((False)) else (((True)))"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTermChar "if False then False else"
-- Failure (interactive):1:25: error: unexpected
--     EOF, expected: "(", "False",
--     "True", "if", lowercase letter,
--     white space
-- if False then False else<EOF>
--                         ^
--
-- >>> parse parseTermChar "if False then False"
-- Failure (interactive):1:20: error: unexpected
--     EOF, expected: "else",
--     white space
-- if False then False<EOF>
--                    ^
--
-- >>> parse parseTermChar "if potato then False else True"
-- Success (TmIf (TmVar "potato") TmFalse TmTrue)
--
-- >>> parse parseTermChar "if potato thenFalseelseTrue"
-- Success (TmIf (TmVar "potato") TmFalse TmTrue)
--
-- >>> parse parseTermChar "if False potato False else True"
-- Failure (interactive):1:10: error: expected: "then",
--     space
-- if False potato False else True<EOF>
--          ^
parseTermChar :: (Monad m, CharParsing m) => m Term
parseTermChar =
    asum parseTermRulesChar <|> p parseTermChar
  where
    p = between (char '(' <* spaces) (spaces <* char ')')

-- |
-- >>> parse parseTermCharName "potato"
-- Success (TmVar "potato")
--
-- >>> parse parseTermCharName "Potato"
-- Failure (interactive):1:1: error: expected: term
-- Potato<EOF>
-- ^
--
-- >>> parse parseTermCharName "False"
-- Success TmFalse
--
-- >>> parse parseTermCharName "false"
-- Success (TmVar "false")
--
-- >>> parse parseTermCharName "True"
-- Success TmTrue
--
-- >>> parse parseTermCharName "true"
-- Success (TmVar "true")
--
-- >>> parse parseTermCharName "(False ) "
-- Success TmFalse
--
-- >>> parse parseTermCharName "False ) "
-- Success TmFalse
--
-- >>> parse parseTermCharName "(False  "
-- Failure (interactive):1:9: error: unexpected
--     EOF, expected: ")", space
-- (False  <EOF>
--         ^
-- >>> parse parseTermCharName "if False then False else True"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTermCharName "if (False) then ((False)) else (((True)))"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTermCharName "if False then False else"
-- Failure (interactive):1:25: error: unexpected
--     EOF, expected: term, white space
-- if False then False else<EOF>
--                         ^
--
-- >>> parse parseTermCharName "if False then False"
-- Failure (interactive):1:20: error: unexpected
--     EOF, expected: "else",
--     white space
-- if False then False<EOF>
--                    ^
--
-- >>> parse parseTermCharName "if potato then False else True"
-- Success (TmIf (TmVar "potato") TmFalse TmTrue)
--
-- >>> parse parseTermCharName "if False potato False else True"
-- Failure (interactive):1:10: error: expected: "then",
--     space
-- if False potato False else True<EOF>
--          ^
parseTermCharName :: (Monad m, CharParsing m) => m Term
parseTermCharName =
  (asum parseTermRulesCharName <|> p parseTermCharName) <?> "term"
  where
    p = between (char '(' <* spaces) (spaces <* char ')')

-- |
-- >>> parse parseTermToken "potato"
-- Success (TmVar "potato")
--
-- >>> parse parseTermToken "Potato"
-- Failure (interactive):1:1: error: expected: "(",
--     "False", "True", "if",
--     identifier
-- Potato<EOF>
-- ^
--
-- >>> parse parseTermToken "False"
-- Success TmFalse
--
-- >>> parse parseTermToken "false"
-- Success (TmVar "false")
--
-- >>> parse parseTermToken "True"
-- Success TmTrue
--
-- >>> parse parseTermToken "true"
-- Success (TmVar "true")
--
-- >>> parse parseTermToken "(False ) "
-- Success TmFalse
--
-- >>> parse parseTermToken "False ) "
-- Success TmFalse
--
-- >>> parse parseTermToken "(False  "
-- Failure (interactive):1:9: error: unexpected
--     EOF, expected: ")"
-- (False  <EOF>
--         ^
--
-- >>> parse parseTermToken "if False then False else True"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTermToken "if (False) then ((False)) else (((True)))"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTermToken "if False then False else"
-- Failure (interactive):1:25: error: unexpected
--     EOF, expected: "(", "False",
--     "True", "if", end of "else",
--     identifier
-- if False then False else<EOF>
--                         ^
--
-- >>> parse parseTermToken "if False then False"
-- Failure (interactive):1:20: error: unexpected
--     EOF, expected: "else",
--     end of "False"
-- if False then False<EOF>
--                    ^
--
-- >>> parse parseTermToken "if potato then False else True"
-- Success (TmIf (TmVar "potato") TmFalse TmTrue)
--
-- >>> parse parseTermToken "if False potato False else True"
-- Failure (interactive):1:10: error: expected: "then"
-- if False potato False else True<EOF>
--          ^
parseTermToken :: (Monad m, TokenParsing m) => m Term
parseTermToken =
  asum parseTermRulesToken <|> parens parseTermToken

-- |
-- >>> parse parseTermTokenName "potato"
-- Success (TmVar "potato")
--
-- >>> parse parseTermTokenName "Potato"
-- Failure (interactive):1:1: error: expected: term
-- Potato<EOF>
-- ^
--
-- >>> parse parseTermTokenName "False"
-- Success TmFalse
--
-- >>> parse parseTermTokenName "false"
-- Success (TmVar "false")
--
-- >>> parse parseTermTokenName "True"
-- Success TmTrue
--
-- >>> parse parseTermTokenName "true"
-- Success (TmVar "true")
--
-- >>> parse parseTermTokenName "(False ) "
-- Success TmFalse
--
-- >>> parse parseTermTokenName "False ) "
-- Success TmFalse
--
-- >>> parse parseTermTokenName "(False  "
-- Failure (interactive):1:9: error: unexpected
--     EOF, expected: ")"
-- (False  <EOF>
--         ^
--
-- >>> parse parseTermTokenName "if False then False else True"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTermTokenName "if (False) then ((False)) else (((True)))"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- >>> parse parseTermTokenName "if False then False else"
-- Failure (interactive):1:25: error: unexpected
--     EOF, expected: end of "else",
--     term
-- if False then False else<EOF>
--                         ^
--
-- >>> parse parseTermTokenName "if False then False"
-- Failure (interactive):1:20: error: unexpected
--     EOF, expected: "else",
--     end of "False"
-- if False then False<EOF>
--                    ^
--
-- >>> parse parseTermTokenName "if potato then False else True"
-- Success (TmIf (TmVar "potato") TmFalse TmTrue)
--
-- >>> parse parseTermTokenName "if False potato False else True"
-- Failure (interactive):1:10: error: expected: "then"
-- if False potato False else True<EOF>
--          ^
-- >>> parse parseTermTokenName "if potato then FalseelseTrue"
-- Success (TmIf (TmVar "potato") TmFalse TmTrue)
--
parseTermTokenName :: (Monad m, TokenParsing m) => m Term
parseTermTokenName =
  (asum parseTermRulesTokenName <|> parens parseTermTokenName)
  <?> "term"
