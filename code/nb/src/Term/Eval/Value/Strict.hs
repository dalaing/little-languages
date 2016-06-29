{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Value rules and helpers for the NB language.

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
valueTmSucc :: (Term -> Maybe Term) -- ^ The value function for the NB language.
            -> Term
            -> Maybe Term
valueTmSucc val (TmSucc tm) =
  TmSucc <$> val tm
valueTmSucc _ _ =
  Nothing

-- | The value rule for 'TmFalse'.
valueTmFalse :: Term
             -> Maybe Term
valueTmFalse TmFalse =
  Just TmFalse
valueTmFalse _ =
  Nothing

-- | The value rule for 'TmTrue'.
valueTmTrue :: Term
            -> Maybe Term
valueTmTrue TmTrue =
  Just TmTrue
valueTmTrue _ =
  Nothing

-- | The set of value rules for the NB language.
--
-- The value rules return a 'Just' value when their input
-- is a value, and a 'Nothing' value otherwise.
valueRules :: [Term -> Maybe Term]
valueRules =
  [ valueTmZero
  , valueTmSucc value
  , valueTmFalse
  , valueTmTrue
  ]

-- | The value function for the NB language.
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
--
-- >>> value TmFalse
-- Just TmFalse
--
-- >>> value (TmIf TmFalse TmFalse TmTrue)
-- Nothing
--
-- >>> value (TmIsZero (TmSucc TmZero))
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
--
-- >>> isValue TmFalse
-- True
--
-- >>> isValue (TmIf TmFalse TmFalse TmTrue)
-- False
--
-- >>> isValue (TmIsZero (TmSucc TmZero))
-- False
isValue :: Term
        -> Bool
isValue =
  isJust .
  value
