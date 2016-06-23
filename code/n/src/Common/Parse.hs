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
  ) where

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc)

-- from 'trifecta'
import           Text.Trifecta.Delta          (Delta (..))
import           Text.Trifecta.Parser         (Parser, parseString)
import           Text.Trifecta.Result         (Result (..))

-- | Runs a 'Parser' over a string and converts the result to an 'Either'.
parseFromString :: Parser a
                -> String
                -> Either Doc a
parseFromString p s =
  case parseString p (Columns 0 0) s of
    Success r -> Right r
    Failure d -> Left d
