{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Types for the NB language.
-}
module Type (
    Type(..)
  ) where

-- | The types in the language NB.
data Type =
    TyNat  -- ^ The type of natural numbers.
  | TyBool -- ^ The type of Booleans.
  deriving (Eq, Ord, Show)
