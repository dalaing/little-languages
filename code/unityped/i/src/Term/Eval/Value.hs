module Term.Eval.Value where

import Control.Lens (preview)

import Term

valueTmInt :: Term -> Maybe Term
valueTmInt = fmap TmInt . preview _TmInt

value :: Term -> Maybe Term
value = valueTmInt
