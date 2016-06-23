module Rules.GenShrink (
    ShrinkResult(..)
  , Shrinkable(..)
  , shrShrink
  , GenShrink(..)
  , fromArbitrary
  , gen
  , shr
  ) where

import Control.Applicative
import Data.Functor.Alt
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup

import Data.Profunctor

import Test.QuickCheck

data AltGen a = AltGen (NEL.NonEmpty (Gen a))

instance Functor AltGen where
  fmap f (AltGen gs) = AltGen $ fmap (fmap f) gs

instance Applicative AltGen where
  pure = AltGen . pure . pure
  AltGen gs1 <*> AltGen gs2 = AltGen $ liftA2 (<*>) gs1 gs2

instance Alt AltGen where
  AltGen gs1 <!> AltGen gs2 = AltGen $ gs1 <> gs2

data ShrinkResult a =
    Pure a
  | Synthetic a

instance Functor ShrinkResult where
  fmap f (Pure x) = Pure (f x)
  fmap f (Synthetic x) = Synthetic (f x)

instance Applicative ShrinkResult where
  pure = Pure
  f <*> x = Synthetic (result f (result x))

instance Monad ShrinkResult where
  return = pure
  x >>= f = Synthetic . result . f . result $ x

synths :: ShrinkResult a -> [a]
synths (Synthetic x) = [x]
synths _ = []

result :: ShrinkResult a -> a
result (Pure x) = x
result (Synthetic x) = x

data Shrinkable t a =
  Shrink (t -> [ShrinkResult a])

instance Profunctor Shrinkable where
  dimap l r (Shrink f) = Shrink (dimap l (fmap (fmap r)) f)

instance Functor (Shrinkable t) where
  fmap = rmap

instance Applicative (Shrinkable t) where
  pure = Shrink . const . pure . pure
  Shrink f <*> Shrink x = Shrink $ \t -> map swap (f t) <*> x t
    where
      swap (Pure y) = Pure . y . result
      swap (Synthetic y) = Synthetic . y . result

instance Alt (Shrinkable t) where
  Shrink x <!> Shrink y = Shrink $ \t -> x t ++ y t

instance Alternative (Shrinkable t) where
  empty = Shrink . const $ []
  (<|>) = (<!>)

instance Monad (Shrinkable t) where
  return = pure
  Shrink x >>= f = Shrink $ \t -> x t >>= (\(Shrink g) -> g t) . f . result

shrAp :: Shrinkable t a -> t -> [a]
shrAp (Shrink f) = map result . f

shrShrink :: Shrinkable t a -> t -> [a]
shrShrink (Shrink f) t = f t >>= synths

{-
instance Contravariant1 Shrinkable where
  contramap1 = lmap

instance ProApply Shrinkable where
  proap scatter gather s1 s2 = Shrink $ \t -> case scatter t of
    (b, c) -> fmap Synthetic (gather <$> shrAp s1 b <*> shrAp s2 c)

instance ProFix Shrinkable where
  profix scatter gather s1 s2 = Shrink $ \t -> case scatter t of
    (b, c) -> fmap Synthetic (shrAp s1 b ++ (gather <$> shrAp s1 b <*> shrAp s2 c))

instance ProAlt Shrinkable where
  proalt scatter gather s1 s2 = Shrink $ \t -> case scatter t of
    Left b -> (Synthetic . gather . Left) <$> shrAp s1 b
    Right c -> (Synthetic . gather . Right) <$> shrAp s2 c
-}

-- eventually we want to be able to encode things like frequency
-- - had a version that reified alt, so that we could list the alternations
--   until they were forced by usage or by an ap
-- - what does that mean for shrink?
--   - it would be good if it could effect the order that shrinks occur, I think
-- - how do we use link/duplicate that information across to Match
data GenShrink t a =
    GenShrink (AltGen a) (Shrinkable t a)

fromArbitrary :: Arbitrary a => GenShrink a a
fromArbitrary = GenShrink (AltGen . pure $ arbitrary) (Shrink $ fmap (fmap Synthetic) shrink)

gen :: GenShrink t a -> Gen a
gen (GenShrink (AltGen g) _) = oneof . NEL.toList $ g

shr :: GenShrink t a -> t -> [a]
shr (GenShrink _ s) t = shrShrink s t

instance Profunctor GenShrink where
  dimap a b (GenShrink g s) = GenShrink (fmap b g) (dimap a b s)

instance Functor (GenShrink t) where
  fmap = rmap

instance Applicative (GenShrink t) where
  pure x = GenShrink (pure x) (pure x)

  GenShrink g1 s1 <*> GenShrink g2 s2 =
    GenShrink (g1 <*> g2) (s1 <*> s2)

instance Alt (GenShrink t) where
  GenShrink g1 s1 <!> GenShrink g2 s2 = GenShrink (g1 <!> g2) (s1 <!> s2)

-- instance Alternative (GenShrink t) where
--  empty = GenShrink empty empty
--  GenShrink g1 s1 <|> GenShrink g2 s2 = GenShrink (oneof [g1, g2]) (s1 <|> s2)

{-
instance Contravariant1 GenShrink where
  contramap1 = lmap

instance ProApply GenShrink where
  proap scatter gather gs1 gs2 =
    let
      GenShrink g1 s1 = normalise gs1
      GenShrink g2 s2 = normalise gs2
    in
      GenShrink (gather <$> g1 <*> g2) (proap scatter gather s1 s2)

-- detecting the base case / doing things based on a size of 0
-- is still not an option, do we need to be careful here?
instance ProFix GenShrink where
  profix scatter gather gs1 gs2 =
    let
      GenShrink g1 s1 = normalise gs1
      GenShrink g2 s2 = normalise gs2
      r1 = sized (\s -> resize (s `div` 2) g1)
    in
      GenShrink (gather <$> r1 <*> g2) (profix scatter gather s1 s2)

instance ProAlt GenShrink where
  proalt scatter gather gs1 gs2 =
    let
     GenShrink g1 s1 = normalise gs1
     GenShrink g2 s2 = normalise gs2
    in
      GenShrink (oneof [fmap (gather . Left) g1,  fmap (gather . Right) g2]) (proalt scatter gather s1 s2)

instance Divide1 GenShrink where
  divide1 scatter = proap scatter (,)

instance Decide1 GenShrink where
  choose1 scatter = proalt scatter id

unpack :: GenShrink t a -> [GenShrink t a]
unpack (GOr gs) = gs >>= unpack
unpack g = pure g

union :: [GenShrink t a] -> GenShrink t a
union fs = GenShrink (oneof (fmap gen fs)) (Shrink (fmap Synthetic <$> foldMap shr fs))

normalise :: GenShrink t a -> GenShrink t a
normalise = union . unpack
 -}
