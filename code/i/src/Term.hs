{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Terms for the I language.
-}
module Term (
    Term(..)
  , size
  , contains
  , subTerms
  ) where

-- | The terms in the I language.
data Term =
    TmInt Int              -- ^ An integer literal.
  | TmAdd Term Term        -- ^ Addition.
  | TmSub Term Term        -- ^ Subtraction.
  | TmMul Term Term        -- ^ Multiplication.
  | TmExp Term Term        -- ^ Exponentiation.
  deriving (Eq, Ord, Show)

-- | Determines the size of a term.
--
-- The size of the term corresponds to the number of constructors used to create it.
--
-- >>> size (TmInt 3)
-- 1
--
-- >>> size (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- 5

-- Could possibly implement this as 1 + length (subTerms tm)
size :: Term
     -> Int
size (TmInt _) =
  1
size (TmAdd tm1 tm2) =
  1 + size tm1 + size tm2
size (TmSub tm1 tm2) =
  1 + size tm1 + size tm2
size (TmMul tm1 tm2) =
  1 + size tm1 + size tm2
size (TmExp tm1 tm2) =
  1 + size tm1 + size tm2

-- | Tests whether on term is contained within another
--
-- >>> (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5))) `contains` (TmInt 2)
-- True
--
-- >>> (TmInt 2) `contains` (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
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
-- >>> subTerms (TmAdd (TmInt 2) (TmInt 5))
-- [TmInt 2,TmInt 5]
--
-- >>> subTerms (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- [TmInt 3,TmAdd (TmInt 2) (TmInt 5),TmInt 2,TmInt 5]
subTerms :: Term
         -> [Term]
subTerms (TmInt _) =
  []
subTerms (TmAdd tm1 tm2) =
  (tm1 : subTerms tm1) ++ (tm2 : subTerms tm2)
subTerms (TmSub tm1 tm2) =
  (tm1 : subTerms tm1) ++ (tm2 : subTerms tm2)
subTerms (TmMul tm1 tm2) =
  (tm1 : subTerms tm1) ++ (tm2 : subTerms tm2)
subTerms (TmExp tm1 tm2) =
  (tm1 : subTerms tm1) ++ (tm2 : subTerms tm2)

