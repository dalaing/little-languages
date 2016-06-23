{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Term where

import Control.Lens (preview, review)
import Control.Lens.Prism (Prism', prism)
import Control.Lens.TH
import Control.Lens.Wrapped
import Control.Monad (ap)

import Bound
import Bound.Scope
import Prelude.Extras

import Type

-- TODO
-- At present, we could do away with TmLoc, since
-- the location information is only used at the type level.

-- We're still using the parameter for Loc and LocTerm.
-- This is also still a good example of threading that extra parameter
-- through the term type. That will come back with `n` in LC / STLC.

-- We'll probably want to refine Loc / LocTerm to be more reusable,
-- since we're going to want source locations for types in STLC, and
-- for bindings in patterns once they show up

data TermF l n f a =
    TmVar a
  | TmApp (f a) (f a)
  | TmLam n (Type l) (Scope () f a)
  | TmFalse
  | TmTrue
  | TmIf (f a) (f a) (f a)
  | TmLoc l (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

hoistTermF :: Functor f => (forall x. f x -> g x) -> TermF l n f a -> TermF l n g a
hoistTermF _ (TmVar x) = TmVar x
hoistTermF g (TmApp f x) = TmApp (g f) (g x)
hoistTermF g (TmLam n t e) = TmLam n t (hoistScope g e)
hoistTermF _ TmFalse = TmFalse
hoistTermF _ TmTrue = TmTrue
hoistTermF g (TmIf t1 t2 t3) = TmIf (g t1) (g t2) (g t3)
hoistTermF g (TmLoc l t) = TmLoc l (g t)

makeClassyPrisms ''TermF

newtype Term l n a = Term { getTerm :: TermF l n (Term l n) a }
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq l, Eq n) => Eq1 (Term l n) where
  (==#) = (==)

instance (Ord l, Ord n) => Ord1 (Term l n) where
  compare1 = compare

instance (Show l, Show n) => Show1 (Term l n) where
  showsPrec1 = showsPrec

instance Applicative (Term l n) where
  pure = return
  (<*>) = ap

instance Monad (Term l n) where
  return = review _TmVar

  Term (TmVar x) >>= g = g x
  Term (TmApp f x) >>= g = Term (TmApp (f >>= g) (x >>= g))
  Term (TmLam n t e) >>= g = Term (TmLam n t (e >>>= g))
  Term TmFalse >>= _ = Term TmFalse
  Term TmTrue >>= _ = Term TmTrue
  Term (TmIf t1 t2 t3) >>= g = Term (TmIf (t1 >>= g) (t2 >>= g) (t3 >>= g))
  Term (TmLoc l t) >>= g = Term (TmLoc l (t >>= g))

makeWrapped ''Term

instance AsTermF (Term l n a) l n (Term l n) a where
  _TermF = _Wrapped

data Loc l n a = Loc { _annotation :: l, _term :: LocTerm l n a}
               deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Monoid l, Eq l, Eq n) => Eq1 (Loc l n) where
  (==#) = (==)

instance (Monoid l, Ord l, Ord n) => Ord1 (Loc l n) where
  compare1 = compare

instance (Show l, Show n) => Show1 (Loc l n) where
  showsPrec1 = showsPrec

instance Monoid l => Applicative (Loc l n) where
  pure = return
  (<*>) = ap

instance Monoid l => Monad (Loc l n) where
  return = Loc mempty . return

  Loc l x >>= g = Loc l (x >>= _term . g)

newtype LocTerm l n a = LocTerm { getLocTerm :: TermF l n (Loc l n) a}
                  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Monoid l => Applicative (LocTerm l n) where
  pure = return
  (<*>) = ap

instance Monoid l => Monad (LocTerm l n) where
  return = LocTerm . TmVar

  LocTerm (TmVar x) >>= g = g x
  LocTerm (TmApp f x) >>= g = LocTerm (TmApp (f >>= Loc mempty . g) (x >>= Loc mempty . g))
  LocTerm (TmLam n t e) >>= g = LocTerm (TmLam n t (e >>>= Loc mempty . g))
  LocTerm TmFalse >>= _ = LocTerm TmFalse
  LocTerm TmTrue >>= _ = LocTerm TmTrue
  LocTerm (TmIf t1 t2 t3) >>= g = LocTerm (TmIf (t1 >>= Loc mempty . g) (t2 >>= Loc mempty . g) (t3 >>= Loc mempty . g))
  LocTerm (TmLoc l t) >>= g = LocTerm (TmLoc l (t >>= Loc l . g))

makeLenses ''Loc
makeWrapped ''LocTerm

instance AsTermF (LocTerm l n a) l n (Loc l n) a where
  _TermF = _Wrapped

class TermLike t i | t -> i, i -> t where
  nextTerm :: i -> t
  unNext :: t -> i

instance TermLike (Term l n a) (Term l n a) where
  nextTerm = id
  unNext = id

instance Monoid l => TermLike (LocTerm l n a) (Loc l n a) where
  nextTerm = _term
  unNext = Loc mempty

-- TODO prisms

{-
locToTerm :: Loc l n a -> Term l n a
locToTerm (Loc l t) = Term (TmLoc l (locTermToTerm t))

locTermToTerm :: LocTerm l n a -> Term l n a
locTermToTerm = Term . fmap locToTerm . getLocTerm

termToLoc :: Term l n a -> Maybe (Loc l n a)
termToLoc (Term (TmLoc l t)) = Loc l <$> termToLocTerm t
termToLoc _ = Nothing

termToLocTerm :: Term l n a -> Maybe (LocTerm l n a)
termToLocTerm = fmap LocTerm . traverse termToLoc . getTerm

-- do we need const in here?
cataTerm :: (TermF l n f b -> b) -> (a -> b) -> Term l n a -> b
cataTerm f g = f . hoistTermF (cataTerm _ g) . fmap g . getTerm

stripLoc :: Term l n a -> Term l n a
stripLoc = cataTerm f
  where
    f TmFalse = Term TmFalse
    f TmTrue = Term TmTrue
    f (TmIf t1 t2 t3) = Term (TmIf t1 t2 t3)
    f (TmLoc _ t) = t

stripNothingLoc :: Term (Maybe l) n a -> Term l n a
stripNothingLoc = cataTerm f
  where
    f TmFalse = Term TmFalse
    f TmTrue = Term TmTrue
    f (TmIf t1 t2 t3) = Term (TmIf t1 t2 t3)
    f (TmLoc (Just l) t) = Term (TmLoc l t)
    f (TmLoc Nothing t) = t

maybeLocTermToTerm :: LocTerm (Maybe l) n a -> Term l n a
maybeLocTermToTerm = stripNothingLoc . Term . fmap locToTerm . getLocTerm

termToMaybeLoc :: Term l n a -> Loc (Maybe l) n a
termToMaybeLoc (Term (TmLoc l t)) = Loc (Just l) (termToMaybeLocTerm t)
termToMaybeLoc t = Loc Nothing (termToMaybeLocTerm t)

termToMaybeLocTerm :: Term l n a -> LocTerm (Maybe l) n a
termToMaybeLocTerm = LocTerm . bimap Just termToMaybeLoc . getTerm

-- TODO would be nice to generalise this across LocTerm
size :: Term l n a -> Int
size = cataTerm f
  where
    f (TmVar _) = 1
    f (TmApp t1 t2) = 1 + t1 + t2
    f (TmLam _ t) = 1 + t
    f TmFalse = 1
    f TmTrue = 1
    f (TmIf t1 t2 t3) = 1 + t1 + t2 + t3
    f (TmLoc _ t) = 1 + t
-}

_lam :: Eq a => Prism' (Term l a a) (a, Type l, Term l a a)
_lam = prism mk match
  where
    mk (x, t, e)        = review _TmLam (x, t, abstract1 x e)
    match m = case preview _TmLam m of
      Just (n, t, e) -> Right (n, t, instantiate1 (review _TmVar n) e)
      Nothing -> Left m

-- g a - > f
-- id or Loc mempty
-- f -> g a
-- id or term

_lam2 :: (Eq a, Monoid l) => Prism' (Loc l a a) (a, Type l, Loc l a a)
_lam2 = prism mk match
  where
    mk (x, t, e)        = Loc mempty $ review _TmLam (x, t, abstract1 x e)
    match (Loc l m) = case preview _TmLam m of
      Just (n, t, e) -> Right (n, t, instantiate1 (Loc mempty (review _TmVar n)) e)
      Nothing -> Left (Loc l m)

size :: Term l a a -> Int
size = size' . getTerm
  where
    size' (TmVar _) = 1
    size' (TmApp t1 t2) = 1 + size t1 + size t2
    size' (TmLam n _ t) = 1 + size (instantiate1 (Term (TmVar n)) t)
    size' TmFalse = 1
    size' TmTrue = 1
    size' (TmIf t1 t2 t3) = 1 + size t1 + size t2 + size t3
    size' (TmLoc _ t) = 1 + size t
