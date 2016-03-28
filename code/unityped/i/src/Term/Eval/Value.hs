module Term.Eval.Value where

import Term

value :: Term -> Maybe Term
value (TmInt i) = Just $ TmInt i
value _ = Nothing
