module Rules.Pattern (
    Pattern(..)
  , gen
  , shr
  , match
  ) where

import Control.Applicative
import Data.Maybe (isJust, isNothing)

import Data.Functor.Alt
import Data.Profunctor
import Test.QuickCheck

import qualified Rules.GenShrink as G
import qualified Rules.Match as M

-- TODO add frequency information in here
-- it goes into GenShrink to jump from `oneof` to `frequency`
-- it goes into Match in order to order the matches before combining
data Pattern t a = Pattern (G.GenShrink t a) (M.Match t a)

gen :: Pattern t a -> Gen a
gen (Pattern g _) = G.gen g

shr :: Pattern t a -> t -> [a]
shr (Pattern g _) = G.shr g

match :: Pattern t a -> t -> Maybe a
match (Pattern _ m) = M.match m

instance Profunctor Pattern where
  dimap a b (Pattern g m) = Pattern (dimap a b g) (dimap a b m)

instance Functor (Pattern t) where
  fmap = rmap

instance Applicative (Pattern t) where
  pure x = Pattern (pure x) (pure x)
  Pattern fg fm <*> Pattern xg xm = Pattern (fg <*> xg) (fm <*> xm)

instance Alt (Pattern t) where
  Pattern g1 m1 <!> Pattern g2 m2 = Pattern (g1 <!> g2) (m1 <!> m2)

propMatch :: Show a => G.GenShrink a a -> M.Match a a -> Property
propMatch g m = forAllShrink (G.gen g) (G.shr g) (isJust . M.match m)

propNoMatch :: Show a => G.GenShrink a a -> M.Match a a -> Property
propNoMatch g m = forAllShrink (G.gen g) (G.shr g) (isNothing . M.match m)

propSelfMatch :: Show a => Pattern a a -> Property
propSelfMatch (Pattern g m) = propMatch g m

propPatternsDistinct :: Show a => [Pattern a a] -> Property
propPatternsDistinct ps = conjoin $ zipWith test gens fns
  where
    patGen (Pattern g _) = g
    patFn (Pattern _ m) = m
    gens = zip (map patGen ps) [0..]
    fns = zip (map patFn ps) [0..]
    test (g, x) (f, y)
      | x == y = propMatch g f
      | otherwise = propNoMatch g f
