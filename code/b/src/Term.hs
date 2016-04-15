{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Terms for the B language
-}
module Term (
    Term(..)
  , size
  , contains
  , subTerms
  ) where

-- | 
data Term =
    TmFalse             -- ^
  | TmTrue              -- ^ 
  | TmIf Term Term Term -- ^
  deriving (Eq, Ord, Show)

-- | 
size :: Term -- ^
     -> Int  -- ^
size TmFalse = 
  1
size TmTrue =
  1
size (TmIf tm1 tm2 tm3) =
  1 + size tm1 + size tm2 + size tm3

-- |
contains :: Term -- ^
         -> Term -- ^
         -> Bool -- ^
contains tm tmC =
  tm == tmC ||
  case tmC of
    TmFalse -> 
      False
    TmTrue -> 
      False
    TmIf tm1 tm2 tm3 ->
      contains tm tm1 || 
      contains tm tm2 || 
      contains tm tm3

-- |
subTerms :: Term   -- ^
         -> [Term] -- ^
subTerms TmFalse = 
  []
subTerms TmTrue = 
  []
subTerms (TmIf tm1 tm2 tm3) =
  [tm1, tm2, tm3] >>= \x -> 
    x : subTerms x

