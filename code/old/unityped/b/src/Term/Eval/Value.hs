module Term.Eval.Value where

import Data.Foldable (asum)

import Term

valueTmFalse :: Term 
             -> Maybe Term
valueTmFalse TmFalse =
  Just TmFalse
valueTmFalse _ =
  Nothing

valueTmTrue :: Term 
            -> Maybe Term
valueTmTrue TmTrue =
  Just TmTrue
valueTmTrue _ =
  Nothing

valueRules :: [Term -> Maybe Term]
valueRules =
  [ valueTmFalse
  , valueTmTrue
  ]

value :: Term
      -> Maybe Term
value tm =
  asum .
  map ($ tm) $
  valueRules
