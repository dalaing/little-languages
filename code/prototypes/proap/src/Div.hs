{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Div where

import Control.Applicative
import Data.Foldable

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Bitraversable
-- import Data.Void
import Generics.Eot
import Data.Functor.Identity
import Data.Functor.Apply
import Data.Functor.Alt

import Data.Profunctor
import Data.Semigroupoid

class Contravariant1 p where
  contramap1 :: (b -> a) -> p a c -> p b c

class Contravariant1 p => Divide1 p where
  divide1 :: (a -> (b,c)) -> p b d -> p c e -> p a (d, e)

class Divide1 p => Divisible1 p where
  conquer1 :: (a -> ()) -> p a ()

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
  conquer2 :: (a -> ()) -> p a ()

class Divide2 p => Decide2 p where
  choose2 :: p a (Either b c) -> p b d -> p c e -> p a (Either d e)

class Decide2 p => Decidable2 p where
  lose2 :: (a -> Void) -> p a Void

instance Contravariant1 (->) where
  contramap1 = lmap

instance Divide1 (->) where
  divide1 split x y = bimap x y . split

instance Divisible1 (->) where
  conquer1 = id

instance Decide1 (->) where
  choose1 split x y = bimap x y . split

instance Decidable1 (->) where
  lose1 = id

instance Divide2 (->) where
  divide2 split x y = bimap x y . split

instance Divisible2 (->) where
  conquer2 = id

instance Decide2 (->) where
  choose2 split x y = bimap x y . split

instance Decidable2 (->) where
  lose2 = id

instance Functor f => Contravariant1 (Star f) where
  contramap1 f (Star g) = Star (lmap f g)

instance Applicative f => Divide1 (Star f) where
  divide1 split (Star x) (Star y) = Star $ bitraverse x y . split

instance Applicative f => Divisible1 (Star f) where
  conquer1 = Star . fmap pure

instance Applicative f => Decide1 (Star f) where
  choose1 split (Star x) (Star y) = Star $ bitraverse x y . split

instance Applicative f => Decidable1 (Star f) where
  lose1 f = Star (pure  . f)

instance Monad f => Divide2 (Star f) where
  divide2 (Star split) (Star x) (Star y) = Star $ split >=> bitraverse x y

instance Monad f => Divisible2 (Star f) where
  conquer2 = Star . fmap pure

instance Monad f => Decide2 (Star f) where
  choose2 (Star split) (Star x) (Star y) = Star $ split >=> bitraverse x y

instance Monad f => Decidable2 (Star f) where
  lose2 f = Star (pure . f)

explode :: (HasEot t, Profunctor p) => p (Eot t) (Eot t) -> p t t
explode = dimap toEot fromEot

class ProApply p where
  proap :: (a -> (b, c))
        -> ((d, e) -> f)
        -> p b d
        -> p c e
        -> p a f

-- if back . there == id
--   p b b -> p c c -> p a a
--
-- proap invC C x y = uncurry C <$> x <*> y
--

-- Apply laws
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- Applicative laws
-- pure id <*> v = v
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u

-- Divisible
-- conquer :: (a -> ()) -> f a
-- - this is always const mempty, so we don't need the first argument?

-- Divisible laws
-- delta a = (a,a)
-- divide delta m conquer = m
-- divide delta conquer m = m
-- divide delta (divide delta m n) o = divide delta m (divide delta n o)

-- Divisible laws
-- divide f m conquer = contramap (fst . f) m
-- divide f conquer m = contramap (snd . f) m
-- divide f (divide g m n) o = divide f' m (divide id n o) where
--   f' a = case f a of (bc,d) -> case g bc of (b,c) -> (a,(b,c))

-- Alt laws
-- (x <|> y) <|> z = x <|> (y <|> z)

-- Alternative laws
-- empty <|> x = x
-- x <|> empty = x

-- Distribution laws for Alternative and Applicative
-- - from http://stackoverflow.com/questions/13080606/confused-by-the-meaning-of-the-alternative-type-class-and-its-relationship-to
-- Right distributivity (of <*>):  (f <|> g) <*> a = (f <*> a) <|> (g <*> a)
-- Right absorption (for <*>):  empty <*> a = empty
-- Left distributivity (of fmap):  f <$> (a <|> b) = (f <$> a) <|> (f <$> b)
-- Left absorption (for fmap):  f <$> empty = empty

-- Decidable
-- lose :: (a -> Void) -> f a

-- Decidable laws
-- choose Left m (lose f)  = m
-- choose Right (lose f) m = m
-- choose f (choose g m n) o = divide f' m (divide id n o) where
--   f' bcd = either (either id (Right . Left) . g) (Right . Right) . f

infixr 4 <<*>>
(<<*>>) :: ProApply p => p b d -> p c e -> p (b,c) (d,e)
(<<*>>) = proap id id

-- infixr 4 <<@>>
-- (<<@>>) :: ProFix p => p b d -> p c e -> p (b,c) (d,e)
-- (<<@>>) = profix id id

strongAp :: (Strong p, Semigroupoid p)
         => p a (b, c)
         -> p (d, e) f
         -> p b d
         -> p c e
         -> p a f
strongAp there back x1 x2 =
  back `o`
  second' x2 `o`
  first' x1 `o`
  there

class ProAlt p where
  proalt :: (a -> Either b c)
         -> (Either d e -> f)
         -> p b d
         -> p c e
         -> p a f

infixr 3 <<|>>
(<<|>>) :: ProAlt p => p b d -> p c e -> p (Either b c) (Either d e)
(<<|>>) = proalt id id

prismAlt :: ProAlt p => Prism s t a b -> p t t -> p a b -> p s t
prismAlt p x y = withPrism p $ \back there -> proalt there (cap . fmap back) x y

choiceAlt :: (Choice p, Semigroupoid p)
          => p a (Either b c)
          -> p (Either d e) f
          -> p b d
          -> p c e
          -> p a f
choiceAlt there back x1 x2 =
  back `o`
  right' x2 `o`
  left' x1 `o`
  there

 -- need conquer here to close things off
 -- this allows us to do
 -- - resizing of recursive Gens
 -- - recursive shrinks that include the shrinks of the subterms as well as the combinations of the shrinks of the arguments
class ProApply p => ProFix p where
  profix :: (a -> (a, c))
         -> ((d, e) -> d)
         -> p a d
         -> p c e
         -> p a d
  profix = proap

instance ProApply (->) where
  proap scatter gather x y a =
    case scatter a of
      (b, c) -> curry gather (x b) (y c)

instance ProAlt (->) where
  proalt scatter gather x y a =
    case scatter a of
      Left b -> gather . Left . x $ b
      Right c -> gather . Right . y $ c

instance Applicative f => ProApply (Star f) where
  proap scatter gather (Star x) (Star y) =
    Star $ fmap gather . bitraverse x y . scatter

instance Applicative f => ProAlt (Star f) where
  proalt scatter gather (Star x) (Star y) =
    Star $ fmap gather . bitraverse x y . scatter

