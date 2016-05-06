{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeOperators #-}
module Extras (
    Eq1(..)
  , Show1(..)
  , Eq2(..)
  , Show2(..)
  , Eq3(..)
  , Show3(..)
  , Monoid2(..)
  ) where

import Data.Constraint

class Eq1 ty where
  spanEq1 :: Eq nTy :- Eq (ty nTy)

class Show1 ty where
  spanShow1 :: Show nTy :- Show (ty nTy)

class Eq2 e where
  spanEq2 :: (Eq n, Eq a) :- Eq (e n a)

class Show2 e where
  spanShow2 :: (Show n, Show a) :- Show (e n a)

class Eq3 tm where
  spanEq3 :: (Eq nTy, Eq nTm, Eq a) :- Eq (tm nTy nTm a)

class Show3 tm where
  spanShow3 :: (Show nTy, Show nTm, Show a) :- Show (tm nTy nTm a)

class Monoid2 ctx where
  spanMonoid2 :: Ord a :- Monoid (ctx n a)
