{-# LANGUAGE FlexibleContexts #-}
module Term.Eval.Value where

import Control.Applicative ((<|>))

import Term

bv :: Term l
   -> Maybe (Term l)
bv TmFalse = Just TmFalse
bv TmTrue = Just TmTrue
bv _ = Nothing

nv :: Term l
   -> Maybe (Term l)
nv TmZero = Just TmZero
nv (TmSucc t) = TmSucc <$> nv t
nv _ = Nothing

value :: Term l
      -> Maybe (Term l)
value t = 
  bv t <|> nv t
