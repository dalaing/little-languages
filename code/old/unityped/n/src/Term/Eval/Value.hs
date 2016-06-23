module Term.Eval.Value where

import Data.Foldable (asum)

import Term

valueTmZero :: Term 
            -> Maybe Term
valueTmZero TmZero =
  Just TmZero
valueTmZero _ =
  Nothing

valueTmSucc :: Term 
            -> Maybe Term
valueTmSucc (TmSucc tm) =
  TmSucc <$> value tm
valueTmSucc _ =
  Nothing

valueRules :: [Term -> Maybe Term]
valueRules = 
  [ valueTmZero
  , valueTmSucc
  ]

value :: Term
      -> Maybe Term
value tm =
  asum .
  map ($ tm) $
  valueRules
