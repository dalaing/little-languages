{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Common.Text (
    ExpressionInfo(..)
  , Assoc(..)
  , combineTables
  ) where

import Text.Parser.Expression (Assoc(..), OperatorTable)

data ExpressionInfo =
  ExpressionInfo {
    _assoc :: Assoc
  , _precedence :: Int
  } deriving (Eq, Ord, Show)

combineTables :: [OperatorTable m a]
              -> OperatorTable m a
combineTables os =
    reverse .
    foldr (zipWith (++) . pad) (pad []) $
    os
  where
    l = maximum . map length $ os
    pad ls = replicate (l - length ls) [] ++ ls
