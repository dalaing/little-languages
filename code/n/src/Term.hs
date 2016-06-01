{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Terms for the N language.
-}
module Term (
    Term(..)
  , size
  , contains
  , subTerms
  ) where

-- | The terms in the language N.
data Term =
    TmZero      -- ^ The natural number zero.
  | TmSucc Term -- ^ The successor of a natural number.
  | TmPred Term -- ^ The predecessor of a natural number.
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
size :: Term
     -> Int
size TmZero =
  1
size (TmSucc tm) =
  1 + size tm
size (TmPred tm) =
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
subTerms :: Term
         -> [Term]
subTerms TmZero =
  []
subTerms (TmSucc tm) =
  tm : subTerms tm
subTerms (TmPred tm) =
  tm : subTerms tm

