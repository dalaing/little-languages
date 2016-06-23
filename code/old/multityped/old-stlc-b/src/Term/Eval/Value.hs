{-# LANGUAGE FlexibleContexts #-}
module Term.Eval.Value where

import Control.Applicative ((<|>))
import Control.Lens (preview)

import Term

bv :: (AsTermF f l n g a, TermLike f (g a))
   => f -> Maybe f
bv t =
  (t <$ preview _TmFalse t) <|>
  (t <$ preview _TmTrue t)

lv :: (AsTermF f l n g a, TermLike f (g a))
   => f -> Maybe f
lv t =
  (t <$ preview _TmLam t)

value :: (AsTermF f l n g a, TermLike f (g a))
      => f
      -> Maybe f
value t = bv t <|> lv t
