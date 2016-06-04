{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Terms for the NB language.
-}
module Term (
    Term(..)
  , size
  , contains
  , subTerms
  ) where

-- | The terms in the language NB.
data Term =
    TmZero              -- ^ The natural number zero.
  | TmSucc Term         -- ^ The successor of a natural number.
  | TmPred Term         -- ^ The predecessor of a natural number.
  | TmFalse             -- ^ The Boolean literal 'false'.
  | TmTrue              -- ^ The Boolean literal 'true'.
  | TmIf Term Term Term -- ^ An if-then-else expression.
  | TmIsZero Term       -- ^ An expression that tests if a Natural number is zero.
  deriving (Eq, Ord, Show)

-- | Determines the size of a term.
--
-- The size of the term corresponds to the number of constructors used to create it.
--
-- >>> size TmZero
-- 1
--
-- >>> size (TmSucc (TmSucc (TmPred TmZero)))
-- 4
--
-- >>> size TmFalse
-- 1
--
-- >>> size (TmIf TmFalse TmFalse TmTrue)
-- 4
--
-- >>> size (TmIsZero (TmSucc (TmSucc (TmPred TmZero))))
-- 5
size :: Term
     -> Int
size TmZero =
  1
size (TmSucc tm) =
  1 + size tm
size (TmPred tm) =
  1 + size tm
size TmFalse =
  1
size TmTrue =
  1
size (TmIf tm1 tm2 tm3) =
  1 + size tm1 + size tm2 + size tm3
size (TmIsZero tm) =
  1 + size tm

-- | Tests whether on term is contained within another
--
-- >>> (TmSucc (TmSucc (TmPred TmZero))) `contains` (TmPred TmZero)
-- True
--
-- >>> (TmPred TmZero) `contains` (TmSucc (TmSucc (TmPred TmZero))) 
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
-- >>> subTerms (TmSucc (TmSucc (TmPred TmZero)))
-- [TmSucc (TmPred TmZero),TmPred TmZero,TmZero]
--
-- >>> subTerms (TmIf TmFalse TmFalse TmTrue)
-- [TmFalse,TmFalse,TmTrue]
--
-- >>> subTerms (TmIf (TmIf TmFalse TmTrue TmFalse) TmFalse TmTrue)
-- [TmIf TmFalse TmTrue TmFalse,TmFalse,TmTrue,TmFalse,TmFalse,TmTrue]
--
-- >>> subTerms (TmIsZero (TmSucc TmZero))
-- [TmSucc TmZero,TmZero]
subTerms :: Term
         -> [Term]
subTerms TmZero =
  []
subTerms (TmSucc tm) =
  tm : subTerms tm
subTerms (TmPred tm) =
  tm : subTerms tm
subTerms TmFalse =
  []
subTerms TmTrue =
  []
subTerms (TmIf tm1 tm2 tm3) =
  [tm1, tm2, tm3] >>= \x ->
    x : subTerms x
subTerms (TmIsZero tm) =
  tm : subTerms tm
