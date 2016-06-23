{-# LANGUAGE FlexibleInstances #-}
module Rules.Match (
    Match()
  , toMatch
  , match
  ) where

import Control.Applicative
import Data.Foldable

import Control.Lens
import Control.Monad
import Data.Functor.Alt
import Data.Bifunctor
import Data.Bitraversable
import Data.Void
import Data.Functor.Identity

import Data.Profunctor

data Match t a =
    Match (t -> Maybe a)
  | MOr [Match t a]

toMatch :: (t -> Maybe a) -> Match t a
toMatch = Match

match :: Match t a -> t -> Maybe a
match (Match m) t = m t
match (MOr ms) t = asum . map (($ t) . match) $ ms

liftMatch2 :: Match t (a, b) -> Match a a' -> Match b b' -> Match t (a', b')
liftMatch2 = undefined

liftMatch3 :: Match t (a, b, c) -> Match a a' -> Match b b' -> Match c c' -> Match t (a', b', c')
liftMatch3 = undefined

instance Profunctor Match where
  dimap a b (Match f) = Match . dimap a (fmap b) $ f
  dimap a b (MOr ms) = MOr . fmap (dimap a b) $ ms

instance Functor (Match t) where
  fmap = rmap

instance Applicative (Match t) where
  pure = Match . pure . pure

  f <*> x =
    let
      Match f' = normalise f
      Match x' = normalise x
    in
      Match (liftA2 (<*>) f' x')

instance Alt (Match t) where
  x <!> y = MOr $ unpack x ++ unpack y

instance Alternative (Match t) where
  empty = MOr []
  (<|>) = (<!>)

unpack :: Match t a -> [Match t a]
unpack (MOr ps) = ps >>= unpack
unpack m = pure m

union :: [Match t a] -> Match t a
union = Match . match . MOr

normalise :: Match t a -> Match t a
normalise = union . unpack
