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

-- |
data ParseRule m t =
    ParseRegular (m t)                     -- ^
  | ParseOp OperatorInfo (m (t -> t -> t)) -- ^

-- |
gatherRegular :: ParseRule m t
              -> Maybe (m t)
gatherRegular (ParseRegular p) =
  Just p
gatherRegular _ =
  Nothing

-- |
gatherOp :: ParseRule m t
         -> Maybe (Int, Operator m t)
gatherOp (ParseOp (OperatorInfo assoc prec) p) =
  Just (prec, Infix p assoc)
gatherOp _ =
  Nothing

-- |
combineTables :: [(Int, Operator m a)]
              -> OperatorTable m a
combineTables =
    fmap (fmap snd) .
    groupBy ((==) `on` fst) .
    sortOn (Down . fst)

-- |
mkParser :: TokenParsing m
         => [ParseRule m t]
         -> m t
mkParser rules =
  let
    parseTerm =
      (<|> parens parseExpr) .
      asum .
      mapMaybe gatherRegular $
      rules
    tables =
      combineTables .
      mapMaybe gatherOp $
      rules
    parseExpr =
      buildExpressionParser tables parseTerm
  in
    parseExpr
