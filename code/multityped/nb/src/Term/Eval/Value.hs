module Term.Eval.Value where

import Control.Applicative ((<|>))

import Term

bv :: Term 
   -> Maybe Term
bv TmFalse = Just TmFalse
bv TmTrue = Just TmTrue
bv _ = Nothing

nv :: Term 
   -> Maybe Term
nv TmZero = Just TmZero
nv (TmSucc t) = TmSucc <$> nv t
nv _ = Nothing

value :: Term 
      -> Maybe Term
value t = 
  bv t <|> nv t
