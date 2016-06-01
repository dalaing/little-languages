{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Terms for the B language.
-}
module Term (
    Term(..)
  , size
  , contains
  , subTerms
  ) where

-- | The terms in the language B.
data Term =
    TmFalse             -- ^ The Boolean literal 'false'.
  | TmTrue              -- ^ The Boolean literal 'true'.
  | TmIf Term Term Term -- ^ An if-then-else expression.
  deriving (Eq, Ord, Show)

-- | Determines the size of a term.
--
-- The size of the term corresponds to the number of constructors used to create it.
--
-- >>> size TmFalse
-- 1
--
-- >>> size (TmIf TmFalse TmFalse TmTrue)
-- 4
size :: Term
     -> Int
size TmFalse =
  1
size TmTrue =
  1
size (TmIf tm1 tm2 tm3) =
  1 + size tm1 + size tm2 + size tm3

-- | Tests whether on term is contained within another
--
-- >>> (TmIf TmFalse TmFalse TmTrue) `contains` TmFalse
-- True
--
-- >>> TmFalse `contains` (TmIf TmFalse TmFalse TmTrue)
-- False
contains :: Term -- ^ The containing term.
         -> Term -- ^ The sub-term to check for.
         -> Bool
contains tm tmC =
  elem tmC . subTerms $ tm

-- | Produces the sub-terms of a term.
--
-- This does not include the term itself.
--
-- >>> subTerms (TmIf TmFalse TmFalse TmTrue)
-- [TmFalse,TmFalse,TmTrue]
--
-- >>> subTerms (TmIf (TmIf TmFalse TmTrue TmFalse) TmFalse TmTrue)
-- [TmIf TmFalse TmTrue TmFalse,TmFalse,TmTrue,TmFalse,TmFalse,TmTrue]
subTerms :: Term
         -> [Term]
subTerms TmFalse =
  []
subTerms TmTrue =
  []
subTerms (TmIf tm1 tm2 tm3) =
  [tm1, tm2, tm3] >>= \x ->
    x : subTerms x

