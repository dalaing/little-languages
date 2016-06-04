{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Types for the I language.
-}
module Type (
    Type(..)
  ) where

-- | The types in the language I.
data Type =
  TyInt -- ^ The type of integers.
  deriving (Eq, Ord, Show)
