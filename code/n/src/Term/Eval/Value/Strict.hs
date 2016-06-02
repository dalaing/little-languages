{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Value rules and helpers for the N language.

This is the strict evaluation variant.
-}
module Term.Eval.Value.Strict (
    valueRules
  , value
  , isValue
  ) where

-- from 'base'
import           Data.Foldable (asum)
import           Data.Maybe    (isJust)

-- local
import           Term          (Term (..))

-- | The value rule for 'TmZero'.
valueTmZero :: Term
            -> Maybe Term
valueTmZero TmZero =
  Just TmZero
valueTmZero _ =
  Nothing

-- | The value rule for 'TmSucc'.
valueTmSucc :: (Term -> Maybe Term) -- ^ The value function for the N language.
            -> Term
            -> Maybe Term
valueTmSucc val (TmSucc tm) =
  TmSucc <$> val tm
valueTmSucc _ _ =
  Nothing

-- | The set of value rules for the N language.
--
-- The value rules return a 'Just' value when their input
-- is a value, and a 'Nothing' value otherwise.
valueRules :: [Term -> Maybe Term]
valueRules =
  [ valueTmZero
  , valueTmSucc value
  ]

-- | The value function for the N language.
--
-- The value function returns a 'Just' value when the
-- given term is a value, and returns 'Nothing' if the
-- given term is not a value.
--
-- This function is built from the contents of 'valueRules'.
--
-- >>> value TmZero
-- Just TmZero
--
-- >>> value (TmSucc TmZero)
-- Just (TmSucc TmZero)
--
-- >>> value (TmPred TmZero)
-- Nothing
--
-- >>> value (TmSucc (TmPred TmZero))
-- Nothing
value :: Term
      -> Maybe Term
value tm =
  asum .
  fmap ($ tm) $
  valueRules

-- | Determines whether a given term is a value.
--
-- >>> isValue TmZero
-- True
--
-- >>> isValue (TmSucc TmZero)
-- True
--
-- >>> isValue (TmPred TmZero)
-- False
--
-- >>> isValue (TmSucc (TmPred TmZero))
-- False
isValue :: Term
        -> Bool
isValue =
  isJust .
  value
