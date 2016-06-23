{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Util.Types where

import Control.Comonad.Cofree


data TermF f l a =
    Var a
  | Add (f a) (f a)
  | Note l (f a)

instance Functor f => Functor (TermF f l) where
  fmap f (Var x) = Var (f x)
  fmap f (Add x y) = Add (fmap f x) (fmap f y)
  fmap f (Note l x) = Note l (fmap f x)

data Term l a = Term { t :: TermF l a (Term l)}

-- functor is const functor, to package up the terms
expand :: Term l a -> Cofree (Const (Term l a)) (Maybe l)
expand = _
-- expand (Term (TmLoc l x)) = Just l :< Const (expand1 x)
-- expand t = Nothing :< Const (expand1 t)


instance Functor (Term l) where
  fmap f (Term t) = Term (fmap f t)

data Fix1 g l a = Fix1 (g (Fix1 g l) l a)

newtype Term2 l a = Term2 (Fix1 TermF l a)


-- we want a bifunctor and the ability to hoist to another functor
-- that hoisting, plus scope from bound, makes me think we want (f a)

data NoteTerm l a = NoteTerm { n :: l, nt :: TermF (NoteTerm l) l a }

data Cofree1 g l a = Cofree1 l (g (Cofree1 g l) l a)

type NoteTerm2 l a = Cofree1 TermF l a

{-
newtype Fix f a = Fix { unFix :: f (Fix f a) }

instance Functor f => Functor (Fix f) where
  fmap f = Fix . fmap (fmap f) . unFix

data Test1F l f = A1 l f | B1 f f

type Test1 l = Fix (Test1F l)


newtype Fix1 f a = Fix1 { unFix1 :: f (Fix1 f) a }

instance Bifunctor f => Functor (Fix1 f) where
  fmap f = Fix1 . bimap (fmap f) f . unFix1

instance Bifoldable f => Foldable (Fix1 f) where
  foldMap f = bifoldMap (foldMap f) f . unFix1

instance Bitraversable f => Traversable (Fix1 f) where
  traverse f = fmap Fix1 . bitraverse (traverse f) f . unFix1




data Cofree1 f a b = Cofree1 { extract1 :: a, unwrap1 :: f (Cofree1 f a b) b }

instance Bifunctor f => Bifunctor (Cofree1 f) where
  bimap f g (Cofree1 a x) = Cofree1 (f a) (bimap (bimap f g) g x)

instance Bifunctor f => Functor (Cofree1 f a) where
  fmap = second

instance Bifoldable f => Bifoldable (Cofree1 f) where
  bifoldMap f g (Cofree1 a x) = f a `mappend` bifoldMap (bifoldMap f g) g x

instance Bifoldable f => Foldable (Cofree1 f a) where
  foldMap = bifoldMap (const mempty)

instance Bitraversable f => Bitraversable (Cofree1 f) where
  bitraverse f g (Cofree1 a x) = Cofree1 <$> f a <*> bitraverse (bitraverse f g) g x

instance Bitraversable f => Traversable (Cofree1 f a) where
  traverse = bitraverse pure


data Test2F f l a = A2 l (f l a) | B2 (f l a) (f l a)

type Test2 l a = Fix1 Test2F  a

type LocTest2 l a = Cofree1 (Test2F l) a


-}
