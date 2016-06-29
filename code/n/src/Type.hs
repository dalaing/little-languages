{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Types for the N language.
-}
module Type (
    Type(..)
  ) where

-- | The types in the N language.
data Type =
  TyNat -- ^ The type of natural numbers.
  deriving (Eq, Ord, Show)
