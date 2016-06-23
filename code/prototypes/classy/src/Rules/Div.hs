{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rules.Div where

import Control.Applicative
import Data.Foldable

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Bitraversable
import Data.Void
import Data.Functor.Identity
import Data.Functor.Apply
import Data.Functor.Alt

import Data.Profunctor

class Contravariant1 p where
  contramap1 :: (b -> a) -> p a c -> p b c

class Contravariant1 p => Divide1 p where
  divide1 :: (a -> (b,c)) -> p b d -> p c e -> p a (d, e)

class Divide1 p => Divisible1 p where
  conquer1 :: p a ()

-- needs to be capped to be able to use alternative here
class Divide1 p => Decide1 p where
  choose1 :: (a -> Either b c) -> p b d -> p c e -> p a (Either d e)

class Decide1 p => Decidable1 p where
  lose1 :: (a -> Void) -> p a Void

cap :: Either a a -> a
cap (Left x) = x
cap (Right x) = x

class Contravariant1 p => Divide2 p where
  divide2 :: p a (b,c) -> p b d -> p c e -> p a (d, e)

class Divide2 p => Divisible2 p where
  conquer2 :: p a ()

class Divide2 p => Decide2 p where
  choose2 :: p a (Either b c) -> p b d -> p c e -> p a (Either d e)

class Decide2 p => Decidable2 p where
  lose2 :: (a -> Void) -> p a Void

instance Contravariant1 (->) where
  contramap1 = lmap

instance Divide1 (->) where
  divide1 split x y = bimap x y . split

instance Divisible1 (->) where
  conquer1 = const ()

instance Decide1 (->) where
  choose1 split x y = bimap x y . split

instance Decidable1 (->) where
  lose1 = id

instance Divide2 (->) where
  divide2 split x y = bimap x y . split

instance Divisible2 (->) where
  conquer2 = const ()

instance Decide2 (->) where
  choose2 split x y = bimap x y . split

instance Decidable2 (->) where
  lose2 = id

instance Functor f => Contravariant1 (Star f) where
  contramap1 f (Star g) = Star (lmap f g)

instance Applicative f => Divide1 (Star f) where
  divide1 split (Star x) (Star y) = Star $ bitraverse x y . split

instance Applicative f => Divisible1 (Star f) where
  conquer1 = Star . const . pure $ ()

instance Applicative f => Decide1 (Star f) where
  choose1 split (Star x) (Star y) = Star $ bitraverse x y . split

instance Applicative f => Decidable1 (Star f) where
  lose1 f = Star (pure  . f)

instance Monad f => Divide2 (Star f) where
  divide2 (Star split) (Star x) (Star y) = Star $ split >=> bitraverse x y

instance Monad f => Divisible2 (Star f) where
  conquer2 = Star . const . pure $ ()

instance Monad f => Decide2 (Star f) where
  choose2 (Star split) (Star x) (Star y) = Star $ split >=> bitraverse x y

instance Monad f => Decidable2 (Star f) where
  lose2 f = Star (pure . f)

class ProApply p where
  proap :: (a -> (b, c)) -> (d -> e -> f) -> p b d -> p c e -> p a f

class ProApply p => ProFix p where
  -- need conquer here to close things off
  -- this allows us to do
  -- - resizing of recursive Gens
  -- - recursive shrinks that include the shrinks of the subterms as well as the combinations of the shrinks of the arguments
  profix :: (a -> (a, c)) -> (d -> e -> d) -> p a d -> p c e -> p a d
  profix = proap

class ProAlt p where
  proalt :: (a -> Either b c) -> (Either d e -> f) -> p b d -> p c e -> p a f

instance Applicative f => ProApply (Star f) where
  proap scatter gather (Star x) (Star y) = Star $ \a -> case scatter a of
    (b,c) -> gather <$> x b <*> y c

instance Functor f => ProAlt (Star f) where
  proalt scatter gather (Star x) (Star y) = Star $ \a -> case scatter a of
    Left b -> (gather . Left) <$> x b
    Right c -> (gather . Right) <$> y c


-- Market a b a (Identity b) -> Market a b s (Identity t)
-- (b -> Identity b, a -> Either (Identity b) a)
-- ->
-- (b -> Identity t, s -> Either (Identity t) a)

 -- Market a a a (Identity a) -> Market a a s (Identity s)
 -- (a -> Identity a, a -> Either (Identity a) a)
 -- ->
 -- (a -> Identity s, s -> Either (Identity s) a)

-- Gen s, t -> Maybe a, a -> s

{-
data Mg a t s = Mg (Maybe s) (t -> Either a s) (a -> Identity s)

-- IntLit              :: Mg Int Term Term
-- TmAdd               :: Mg (Term, Term) Term Term
-- TmAdd IntLit IntLit :: Mg (Int, Int) Term Term

-- TmAdd a b :: Mg (c, d) Term Term
-- a :: Mg c Term Term
-- b :: Mg d Term Term

instance Profunctor (Mg a) where
  dimap l r (Mg g t b) = Mg (fmap r g) (dimap l (fmap r) t) (fmap r . b)

instance Contravariant1 (Mg a) where
  contramap1 = lmap

instance Functor (Mg a t) where
  fmap = rmap

instance Applicative (Mg a t) where
  pure x = Mg (pure x) (const . Right $ x) (const . Identity $ x)
  Mg g1 t1 b1 <*> Mg g2 t2 b2 = Mg (g1 <*> g2) (liftA2 (<*>) t1 t2) (liftA2 (<*>) b1 b2)

instance ProApply (Mg a) where
  proap scatter gather (Mg g1 t1 b1) (Mg g2 t2 b2) =
    Mg
      (gather <$> g1 <*> g2)
      (runStar $ proap scatter gather (Star t1) (Star t2))
      (\a -> gather <$> b1 a <*> b2 a)

instance ProAlt (Mg a) where
  proalt scatter gather (Mg g1 t1 b1) (Mg g2 t2 b2) =
    Mg
      (fmap (gather . Left) g1 <|> fmap (gather . Right) g2)
      (runStar $ proalt scatter gather (Star t1) (Star t2))
      (\a -> (fmap (gather . Left) (b1 a) <|> fmap (gather . Right) (b2 a)))
-}

-- data LamePrism a b s t = LP (b -> Identity t) (s -> Either t a)

pDivide :: forall s a b c d. Prism' s (a,b) -> Prism' a c -> Prism' b d -> Prism' s (c, d)
pDivide p0 p1 p2 = 
  withPrism p0 $ \b0 t0 -> 
  withPrism p1 $ \b1 t1 -> 
  withPrism p2 $ \b2 t2 -> 
    let
      d1 :: s -> a -> Either s c
      d1 s a = first (const s) (t1 a)
      d2 :: s -> b -> Either s d
      d2 s b = first (const s) (t2 b)
    in
      prism 
        (lmap (bimap b1 b2) b0)
        (\s -> ($ s) . runStar $ divide2 (Star t0) (Star (d1 s)) (Star (d2 s))) 

-- lDivide :: forall s a b c d. Lens' s (a,b) -> Lens' a c -> Lens' b d -> Lens' s (c, d)
-- lDivide l0 l1 l2 df s = l0 (divide1 id _ _) s

-- Divide1 / Decide1 / Divide2 / Decide2 instances here for Iso (or
-- whatever concrete type we can us in place of an iso)

iDivide :: Iso' s (a, b) -> Iso' a c -> Iso' b d -> Iso' s (c, d)
iDivide split left right = undefined

iDecide :: Iso' s (Either a b) -> Iso' a c -> Iso' b d -> Iso' s (Either c d)
iDecide split left right = undefined


