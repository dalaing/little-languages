{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GenShrink (
    ShrinkResult(..)
  , Shrinkable(..)
  , shrShrink
  , NewGen()
  , toGen
  , fromGen
  , GenShrink(..)
  , fromArbitrary
  , gen
  , shr
  ) where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Functor.Alt
import Data.Semigroup

import Data.Profunctor

import Test.QuickCheck

import Div

-- would be nice to have combinators to scale this
-- maybe after we try to find a way to make that frequency-tagging composable
newtype NewGen a = MkNewGen { unNewGen :: [(Int, Gen a)]}

fromGen :: Gen a -> NewGen a
fromGen g = MkNewGen [(1, g)]

toGen :: NewGen a -> Maybe (Gen a)
toGen (MkNewGen []) = Nothing
toGen (MkNewGen xs) = Just $ frequency xs

instance Functor NewGen where
  fmap f (MkNewGen gs) = MkNewGen $ fmap (fmap (fmap f)) gs

instance Applicative NewGen where
  pure = MkNewGen . pure . (1,) . pure
  MkNewGen f <*> MkNewGen x = MkNewGen (combine <$> f <*> x)
    where
      combine (f1, g1) (f2, g2) = (f1 * f2, g1 <*> g2)

instance Alternative NewGen where
  empty = MkNewGen []
  MkNewGen x <|> MkNewGen y = MkNewGen $ x ++ y

-- TODO Monad instance
-- - not sure we can do that with the Gen / NewGen distinction in place

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

instance Contravariant1 Shrinkable where
  contramap1 = lmap

instance ProApply Shrinkable where
  proap scatter gather s1 s2 = Shrink $ \t -> case scatter t of
    (b, c) -> fmap Synthetic (curry gather <$> shrAp s1 b <*> shrAp s2 c)

instance Divide1 Shrinkable where
  divide1 scatter = proap scatter id

instance Divisible1 Shrinkable where
  conquer1 f = undefined

instance Decide1 Shrinkable where
  choose1 scatter = proalt scatter id

instance Decidable1 Shrinkable where
  lose1 f = undefined

instance ProFix Shrinkable where
  profix scatter gather s1 s2 = Shrink $ \t -> case scatter t of
    (b, c) -> fmap Synthetic (shrAp s1 b ++ (curry gather <$> shrAp s1 b <*> shrAp s2 c))

instance ProAlt Shrinkable where
  proalt scatter gather s1 s2 = Shrink $ \t -> case scatter t of
    Left b -> (Synthetic . gather . Left) <$> shrAp s1 b
    Right c -> (Synthetic . gather . Right) <$> shrAp s2 c

-- eventually we want to be able to encode things like frequency
-- - had a version that reified alt, so that we could list the alternations
--   until they were forced by usage or by an ap
-- - what does that mean for shrink?
--   - it would be good if it could effect the order that shrinks occur, I think
-- - how do we use link/duplicate that information across to Match
data GenShrink t a =
    GenShrink (NewGen a) (Shrinkable t a)

fromArbitrary :: Arbitrary a => GenShrink a a
fromArbitrary = GenShrink (MkNewGen . pure . (1,) $ arbitrary) (Shrink $ fmap (fmap Synthetic) shrink)

gen :: GenShrink t a -> Gen a
gen (GenShrink g _) = fromMaybe (error "empty gen") . toGen $ g

shr :: GenShrink t a -> t -> [a]
shr (GenShrink _ s) = shrShrink s

instance Profunctor GenShrink where
  dimap a b (GenShrink g s) = GenShrink (fmap b g) (dimap a b s)

instance Functor (GenShrink t) where
  fmap = rmap

instance Applicative (GenShrink t) where
  pure x = GenShrink (pure x) (pure x)

  GenShrink g1 s1 <*> GenShrink g2 s2 =
    GenShrink (g1 <*> g2) (s1 <*> s2)

instance Alternative (GenShrink t) where
  empty = GenShrink empty empty
  GenShrink g1 s1 <|> GenShrink g2 s2 = GenShrink (g1 <|> g2) (s1 <|> s2)

instance Contravariant1 GenShrink where
  contramap1 = lmap

instance Divide1 GenShrink where
  divide1 scatter = proap scatter id

instance Divisible1 GenShrink where
  conquer1 f = GenShrink (pure ()) (conquer1 f)

instance Decide1 GenShrink where
  choose1 scatter = proalt scatter id

instance Decidable1 GenShrink where
  lose1 f = GenShrink (pure $ error "void") (lose1 f)

instance ProApply GenShrink where
  proap scatter gather (GenShrink g1 s1) (GenShrink g2 s2) =
      GenShrink (curry gather <$> g1 <*> g2) (proap scatter gather s1 s2)

-- detecting the base case / doing things based on a size of 0
-- is still not an option, do we need to be careful here?
instance ProFix GenShrink where
  profix scatter gather (GenShrink (MkNewGen g1) s1) (GenShrink g2 s2) =
    let
      r1 = MkNewGen . fmap (fmap (\x -> sized (\s -> resize (s `div` 2) x))) $ g1
    in
      GenShrink (curry gather <$> r1 <*> g2) (profix scatter gather s1 s2)

instance ProAlt GenShrink where
  proalt scatter gather (GenShrink g1 s1) (GenShrink g2 s2) =
      GenShrink (fmap (gather . Left) g1 <|> fmap (gather . Right) g2) (proalt scatter gather s1 s2)

 {-
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
