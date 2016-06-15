{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Helpers for both parsing and pretty printing.
-}
module Common.Text (
  OperatorInfo(..)
  ) where

-- from 'parsers'
import Text.Parser.Expression (Assoc(..))

-- | Information about operators, for use by parsers and pretty printers.
data OperatorInfo =
  OperatorInfo {
    assoc      :: Assoc -- ^ The associativity of the operator
  , precedence :: Int   -- ^ The precedence of the operator
  } deriving (Eq, Ord, Show)

