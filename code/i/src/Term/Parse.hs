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

-- from 'unordered-containers'
import qualified Data.HashSet                as HS

-- from 'parsers'
import           Text.Parser.Char            (oneOf)
import           Text.Parser.Combinators     ((<?>))
import           Text.Parser.Token           (IdentifierStyle (..),
                                              TokenParsing, reserve, integer)
import           Text.Parser.Expression      (Assoc(..))

-- local
import           Common.Text                 (OperatorInfo(..))
import           Common.Parse                (ParseRule(..), mkParser)
import           Term                        (Term (..))

-- $setup
-- >>> import Text.Trifecta.Parser
-- >>> import Text.Trifecta.Result
-- >>> import Text.Trifecta.Delta
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let parse p s = case parseString p (Lines 0 0 0 0) s of Failure d -> Failure (plain d); Success s -> Success s

-- | The tokenizer style for operators in the I language.
operatorStyle :: TokenParsing m
              => IdentifierStyle m
operatorStyle =
    IdentifierStyle {
      _styleName              = "operator"
    , _styleStart             = _styleLetter operatorStyle
    , _styleLetter            = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , _styleReserved          = HS.fromList reservedOperators
    , _styleHighlight         = Operator
    , _styleReservedHighlight = ReservedOperator
    }
  where
    reservedOperators =
      [ "+"
      , "-"
      , "*"
      , "^"
      ]

-- | A helper function to parse a reserved operator.
reservedOperator :: ( Monad m
                    , TokenParsing m
                    )
                 => String
                 -> m ()
reservedOperator =
  reserve operatorStyle


-- | A parser for 'TmInt'.
--
-- >>> parse parseTmInt "3"
-- Success (TmInt 3)
--
-- >>> parse parseTmInt "false"
-- Failure (interactive):1:1: error: expected: Int
-- false<EOF>
-- ^
parseTmInt :: ( Monad m
              , TokenParsing m
              )
           => m Term
parseTmInt =
  (TmInt . fromInteger) <$> integer
  <?> "Int"

-- | A parser for 'TmAdd'.

-- TODO package up with operator metadata, build table for just this (with parseTerm for base case)
-- use doctest to check / demonstrate associativity
parseTmAdd :: ( Monad m
              , TokenParsing m
              )
           => m (Term -> Term -> Term)
parseTmAdd =
  TmAdd <$ reservedOperator "+"
  <?> "+"

-- | A parser for 'TmSub'.
parseTmSub :: ( Monad m
              , TokenParsing m
              )
           => m (Term -> Term -> Term)
parseTmSub =
  TmSub <$ reservedOperator "-"
  <?> "-"

-- | A parser for 'TmMul'.
parseTmMul :: ( Monad m
              , TokenParsing m
              )
           => m (Term -> Term -> Term)
parseTmMul =
  TmMul <$ reservedOperator "*"
  <?> "*"

-- | A parser for 'TmExp'.
parseTmExp :: ( Monad m
              , TokenParsing m
              )
           => m (Term -> Term -> Term)
parseTmExp =
  TmExp <$ reservedOperator "^"
  <?> "^"

-- | The set of parsing rules for terms of the I language.
parseTermRules :: ( Monad m
                  , TokenParsing m
                  )
               => [ParseRule m Term]
parseTermRules =
  [ ParseRegular parseTmInt
  , ParseOp (OperatorInfo AssocLeft 6) parseTmAdd
  , ParseOp (OperatorInfo AssocLeft 6) parseTmSub
  , ParseOp (OperatorInfo AssocLeft 7) parseTmMul
  , ParseOp (OperatorInfo AssocRight 8) parseTmExp
  ]

-- | The parser for terms of the I language.
--
-- This function is built from the contents of 'parseTermRules',
-- with added support for parentheses.
--
-- We can parse all of the simple forms of the terms:
-- >>> parse parseTerm "3"
-- Success (TmInt 3)
--
-- >>> parse parseTerm "2 + 5"
-- Success (TmAdd (TmInt 2) (TmInt 5))
--
-- >>> parse parseTerm "2 - 5"
-- Success (TmSub (TmInt 2) (TmInt 5))
--
-- >>> parse parseTerm "2 * 5"
-- Success (TmMul (TmInt 2) (TmInt 5))
--
-- >>> parse parseTerm "2 ^ 5"
-- Success (TmExp (TmInt 2) (TmInt 5))
--
-- The left associative operators group to the left:
-- >>> parse parseTerm "3 - 2 + 5"
-- Success (TmAdd (TmSub (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- So brackets grouping things on the left are redundant:
-- >>> parse parseTerm "(3 - 2) + 5"
-- Success (TmAdd (TmSub (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- We need brackets if we want to group things on the right:
-- >>> parse parseTerm "3 - (2 + 5)"
-- Success (TmSub (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
--
-- The right associative operator groups to the right:
-- >>> parse parseTerm "3 ^ 2 ^ 5"
-- Success (TmExp (TmInt 3) (TmExp (TmInt 2) (TmInt 5)))
--
-- So brackets grouping things on the right are redundant:
-- >>> parse parseTerm "3 ^ (2 ^ 5)"
-- Success (TmExp (TmInt 3) (TmExp (TmInt 2) (TmInt 5)))
--
-- We need brackets if we want to group things on the left:
-- >>> parse parseTerm "(3 ^ 2) ^ 5"
-- Success (TmExp (TmExp (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- Multiplication binds more tightly than addition:
-- >>> parse parseTerm "3 * 2 + 5"
-- Success (TmAdd (TmMul (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- >>> parse parseTerm "3 + 2 * 5"
-- Success (TmAdd (TmInt 3) (TmMul (TmInt 2) (TmInt 5)))
--
-- So we need to use brackets multiply by the sum of two terms:
-- >>> parse parseTerm "3 * (2 + 5)"
-- Success (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
--
-- >>> parse parseTerm "(3 + 2) * 5"
-- Success (TmMul (TmAdd (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- Exponentiation binds more tightly than multiplication:
-- >>> parse parseTerm "3 ^ 2 * 5"
-- Success (TmMul (TmExp (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- >>> parse parseTerm "3 * 2 ^ 5"
-- Success (TmMul (TmInt 3) (TmExp (TmInt 2) (TmInt 5)))
--
-- So we need to use brackets if the exponent or the power is a product (or sum) of two terms:
-- >>> parse parseTerm "3 ^ (2 * 5)"
-- Success (TmExp (TmInt 3) (TmMul (TmInt 2) (TmInt 5)))
--
-- >>> parse parseTerm "(3 * 2) ^ 5"
-- Success (TmExp (TmMul (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- Nonsense is still rejected:
-- >>> parse parseTerm "potato"
-- Failure (interactive):1:1: error: expected: term
-- potato<EOF>
-- ^
--
-- Even if you try to hide it in brackets:
-- >>> parse parseTerm "((potato))"
-- Failure (interactive):1:3: error: expected: operator
-- ((potato))<EOF>
--   ^
parseTerm :: ( Monad m
             , TokenParsing m
             )
          => m Term
parseTerm =
  mkParser parseTermRules <?> "term"

