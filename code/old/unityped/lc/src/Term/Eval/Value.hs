module Term.Eval.Value where

import Data.Foldable (asum)

import Term

-- TODO this is not really a variable, at least if we're generating closed
-- terms, perhaps need to fix up the generation or the tests
valueTmVar :: Term n a 
           -> Maybe (Term n a)
valueTmVar (TmVar x) =
  Just $ TmVar x
valueTmVar _ =
  Nothing

valueTmLam :: Term n a 
           -> Maybe (Term n a)
valueTmLam (TmLam n e) =
  Just $ TmLam n e
valueTmLam _ =
  Nothing

valueRules :: [Term n a -> Maybe (Term n a)]
valueRules =
  [ valueTmVar
  , valueTmLam
  ]

value :: Term n a
      -> Maybe (Term n a)
value tm =
  asum .
  map ($ tm) $
  valueRules
