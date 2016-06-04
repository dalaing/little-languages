{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Value rules and helpers for the I language.
-}
module Term.Eval.Value (
    valueRules
  , value
  , isValue
  ) where

-- from 'base'
import           Data.Foldable (asum)
import           Data.Maybe    (isJust)

-- local
import           Term          (Term (..))

-- | The value rule for 'TmInt'.
valueTmInt :: Term
             -> Maybe Term
valueTmInt (TmInt i) =
  Just (TmInt i)
valueTmInt _ =
  Nothing

-- | The set of value rules for the I language.
--
-- The value rules return a 'Just' value when their input
-- is a value, and a 'Nothing' value otherwise.
valueRules :: [Term -> Maybe Term]
valueRules =
  [valueTmInt]

-- | The value function for the I language.
--
-- The value function returns a 'Just' value when the
-- given term is a value, and returns 'Nothing' if the
-- given term is not a value.
--
-- This function is built from the contents of 'valueRules'.
--
-- >>> value (TmInt 3)
-- Just (TmInt 3)
--
-- >>> value (TmAdd (TmInt 2) (TmInt 5))
-- Nothing
value :: Term
      -> Maybe Term
value tm =
  asum .
  fmap ($ tm) $
  valueRules

-- | Determines whether a given term is a value.
--
-- >>> isValue (TmInt 3)
-- True
--
-- >>> isValue (TmAdd (TmInt 2) (TmInt 5))
-- False
isValue :: Term
        -> Bool
isValue =
  isJust .
  value
