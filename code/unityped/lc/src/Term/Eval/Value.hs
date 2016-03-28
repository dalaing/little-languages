module Term.Eval.Value where

import Term

value :: Term n a
      -> Maybe (Term n a)
value (TmVar x) =
  Just $ TmVar x
value (TmLam n e) =
  Just $ TmLam n e
value _ =
  Nothing
