{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Common.Gen (
    sizesEq
  , sizesLte
  ) where

-- from 'base'
import           Control.Monad   (foldM)
import           Data.List       (permutations)

-- from 'QuickCheck'
import           Test.QuickCheck (Gen, choose, elements)


{-
We should care about the distribution here

Picking 2 that sum to s
-- x has expected value of s / 2
x <- choose (0, s)
-- y has expected value of s / 2
y = s - x

Picking 3 that sum to s
-- x has expected value of s / 3
x <- choose (0, 2 / 3 * s)
-- y has expected value of s / 3
y <- choose (0, s - x)
-- z has expected value of s / 3
z = s - x - y

Picking 4 that sum to s
-- x has expected value of s / 4
x <- choose (0, 2 / 4 * s)
-- y has expected value of s / 4
y <- choose (0, 2 / 3 * (s - x))
-- z has expected value of s / 4
z <- choose (0, s - x - y)
-- a has expected value of s / 4
a = s - x - y -z
-}


type Size = Int

sizesEq' :: Int
         -> Size
         -> Gen [Size]
sizesEq' n s
  | n <= 0 =
    return [s]
  | otherwise = do
      -- x <- fmap (`min` s) $ choose (0, 2 * s `div` n)
      x <- choose (0, s)
      xs <- sizesEq' (n - 1) (s - x)
      return $ x : xs

sizesEq :: Int
        -> Size
        -> Gen [Size]
sizesEq n s = do
  xs <- sizesEq' (n - 1) s
  -- TODO replace with shuffleM from random-shuffle
  elements . permutations $ xs

sizesLte' :: Int
          -> Size
          -> Gen [Size]
sizesLte' n s
  | n <= 0 = do
    x <- choose (0, s)
    return [x]
  | otherwise = do
      -- x <- fmap (`min` s) $ choose (0, 2 * s `div` n)
      x <- choose (0, s)
      xs <- sizesEq' (n - 1) (s - x)
      return $ x : xs

sizesLte :: Int
         -> Size
         -> Gen [Size]
sizesLte n s = do
  xs <- sizesLte' (n - 1) s
  -- TODO replace with shuffleM from random-shuffle
  elements . permutations $ xs
