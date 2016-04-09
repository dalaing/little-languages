module Term.Eval.Value where

import Data.Foldable (asum)

import Term

valueTmInt :: Term 
           -> Maybe Term
valueTmInt (TmInt i) = 
  Just $ TmInt i
valueTmInt _ = 
  Nothing

valueRules :: [Term -> Maybe Term]
valueRules =
  [valueTmInt]

value :: Term 
      -> Maybe Term
value tm =
  asum .
  map ($ tm) $
  valueRules
