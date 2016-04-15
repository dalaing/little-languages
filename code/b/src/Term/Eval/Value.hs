{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Value rules and helpers for the B language.
-}
module Term.Eval.Value (
    valueRules
  -- , namedValueRules
  , value
  , isValue
  ) where

import Data.Maybe (isJust)
import Data.Foldable (asum)

import Term (Term(..))

-- |
valueTmFalse :: Term       -- ^
             -> Maybe Term -- ^
valueTmFalse TmFalse =
  Just TmFalse
valueTmFalse _ =
  Nothing

-- |
valueTmTrue :: Term       -- ^
            -> Maybe Term -- ^
valueTmTrue TmTrue =
  Just TmTrue
valueTmTrue _ =
  Nothing

-- |
valueRules :: [Term -> Maybe Term] -- ^
valueRules =
  [ valueTmFalse
  , valueTmTrue
  ]

-- |
value :: Term       -- ^
      -> Maybe Term -- ^
value tm =
  asum .
  fmap ($ tm) $
  valueRules

-- |
isValue :: Term -- ^
        -> Bool -- ^
isValue =
  isJust .
  value
