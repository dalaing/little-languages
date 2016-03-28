module Term.Eval.Value where

import Term

value :: Term
      -> Maybe Term
value TmZero =
  Just TmZero
value (TmSucc n) =
  fmap TmSucc (value n)
value _ =
  Nothing

