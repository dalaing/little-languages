module Term.Eval.Value where

import Control.Applicative ((<|>))
import Control.Lens (preview)

import Term

valueTmFalse :: Term -> Maybe Term
valueTmFalse = fmap (const TmFalse) . preview _TmFalse

valueTmTrue :: Term -> Maybe Term
valueTmTrue = fmap (const TmTrue) . preview _TmTrue

value :: Term -> Maybe Term
value t = valueTmFalse t <|> valueTmTrue t
