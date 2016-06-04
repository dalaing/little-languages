{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Common.Text (
  OperatorInfo(..)
  ) where

-- from 'parsers'
import Text.Parser.Expression (Assoc(..))

-- |
data OperatorInfo =
  OperatorInfo {
    _assoc      :: Assoc
  , _precedence :: Int
  } deriving (Eq, Ord, Show)

