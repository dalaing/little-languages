module Term.Eval.Value where

import Control.Applicative ((<|>))
import Control.Lens (preview)

import Term

valueTmZero :: Term
            -> Maybe Term
valueTmZero =
  fmap (const TmZero) . preview _TmZero

valueTmSucc :: (Term -> Maybe Term)
            -> Term
            -> Maybe Term
valueTmSucc v t = do
  t' <- preview _TmSucc t
  t'' <- v t'
  return . TmSucc $ t''

value :: Term
      -> Maybe Term
value t =
  valueTmZero t <|>
  valueTmSucc value t

