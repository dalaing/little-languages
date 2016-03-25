module Term.Eval.Value where

import Control.Applicative ((<|>))
import Control.Lens (preview)

import Term

valueTmVar :: Term n a -> Maybe (Term n a)
valueTmVar = fmap TmVar . preview _TmVar

valueTmLam :: Term n a -> Maybe (Term n a)
valueTmLam = fmap (uncurry TmLam) . preview _TmLam

value :: Term n a -> Maybe (Term n a)
value t = valueTmVar t <|> valueTmLam t
