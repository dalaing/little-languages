module Term.Eval.Value where

import Term

value :: Term
      -> Maybe Term
value TmFalse = Just TmFalse
value TmTrue = Just TmTrue
value _ = Nothing
