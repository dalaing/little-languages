{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Helpers for parsing.
-}
module Common.Parse (
    parseFromString
  , ParseRule(..)
  , mkParser
  ) where

-- from 'base'
import           Control.Applicative          ((<|>))
import           Data.Foldable                (asum)
import           Data.Function                (on)
import           Data.List                    (groupBy, sortOn)
import           Data.Maybe                   (mapMaybe)
import           Data.Ord                     (Down (..))

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc)

-- from 'trifecta'
import           Text.Trifecta.Delta          (Delta (..))
import           Text.Trifecta.Parser         (Parser, parseString)
import           Text.Trifecta.Result         (Result (..))

-- from 'parsers'
import           Text.Parser.Expression       (Operator (..), OperatorTable,
                                               buildExpressionParser)
import           Text.Parser.Token            (TokenParsing, parens)

-- local
import           Common.Text                  (OperatorInfo (..))

-- | Runs a 'Parser' over a string and converts the result to an 'Either'.
parseFromString :: Parser a
                -> String
                -> Either Doc a
parseFromString p s =
  case parseString p (Lines 0 0 0 0) s of
    Success r -> Right r
    Failure d -> Left d

-- | Rules for parsing languages.
--
-- The intent is that 'm' is some kind of parsing monad, and that 'a' is the type being parsed.
data ParseRule m a =
    ParseRegular (m a)                     -- ^ A parser for 'a'.
  | ParseOp OperatorInfo (m (a -> a -> a)) -- ^ A rule for parsing an infix binary operator on 'a', made up of information about the operator and the combination of a parser for the operator and a the constructor for the operator in 'a'.

-- | Gathers the regular parsing rules.
gatherRegular :: ParseRule m a
              -> Maybe (m a)
gatherRegular (ParseRegular p) =
  Just p
gatherRegular _ =
  Nothing

-- | Gathers the parsing rules for operators.
gatherOp :: ParseRule m a
         -> Maybe (Int, Operator m a)
gatherOp (ParseOp (OperatorInfo a prec) parser) =
  Just (prec, Infix parser a)
gatherOp _ =
  Nothing

-- | Combines information about parsing rules for operators into an 'OperatorTable'.
createTable :: [(Int, Operator m a)]
              -> OperatorTable m a
createTable =
    fmap (fmap snd) .
    groupBy ((==) `on` fst) .
    sortOn (Down . fst)

-- | Combines a list of parsing rules into a parser.
mkParser :: TokenParsing m
         => [ParseRule m a]
         -> m a
mkParser rules =
  let
    parseRegular =
      (<|> parens parseTerm) .
      asum .
      mapMaybe gatherRegular $
      rules
    tables =
      createTable .
      mapMaybe gatherOp $
      rules
    parseTerm =
      buildExpressionParser tables parseRegular
  in
    parseTerm
