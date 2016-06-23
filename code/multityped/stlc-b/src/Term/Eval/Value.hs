{-# LANGUAGE FlexibleContexts #-}
module Term.Eval.Value where

import Control.Applicative ((<|>))

import Term

bv :: Term n l a
   -> Maybe (Term n l a)
bv TmFalse = Just TmFalse
bv TmTrue = Just TmTrue
bv _ = Nothing

lv :: Term n l a
   -> Maybe (Term n l a)
lv l@TmLam{} = Just l
lv _ = Nothing

value :: Term n l a
      -> Maybe (Term n l a)
value t =
  bv t <|> lv t
