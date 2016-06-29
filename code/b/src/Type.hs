{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Types for the B language.
-}
module Type (
    Type(..)
  ) where

-- | The types in the B language.
data Type =
  TyBool -- ^ The type of Booleans.
  deriving (Eq, Ord, Show)
