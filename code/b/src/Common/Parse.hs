{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Common.Parse (
    parseFromString
  ) where

import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Text.Trifecta.Delta          (Delta (..))
import           Text.Trifecta.Parser         (Parser, parseString)
import           Text.Trifecta.Result         (Result (..))

-- |
parseFromString :: Parser a     -- ^
                -> String       -- ^
                -> Either Doc a -- ^
parseFromString p s =
  case parseString p (Lines 0 0 0 0) s of
    Success r -> Right r
    Failure d -> Left d
