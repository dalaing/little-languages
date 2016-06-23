{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Term.Component.Num where

import Control.Lens

import Control.Error.Util

import Bound.Class
import Test.QuickCheck

import Type.Component.Int

import Term.Value

import Term.Component.Int

data TmNum t f a =
    TmAdd (f a) (f a)
  | TmSub (f a) (f a)
  | TmMul (f a) (f a)
  | TmNeg (f a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''TmNum

type WithTmNum t f a = (AsTyInt t, AsTmNum (f a) t f a)

instance Bound (TmNum t) where
  TmAdd x y >>>= g = TmAdd (x >>= g) (y >>= g)
  TmSub x y >>>= g = TmSub (x >>= g) (y >>= g)
  TmMul x y >>>= g = TmMul (x >>= g) (y >>= g)
  TmNeg x   >>>= g = TmNeg (x >>= g)

instance IsValue (TmNum t f a) where
  isValue = const False

-- Patterns

-- do we want either here instead of maybe?
-- probably not, since we want to track failing matches
data Pattern a s t = Pattern {
    scatter :: s -> Maybe a
  , gather :: a -> t
  }

instance Profunctor (Pattern a) where
  dimap l r (Pattern s g) = Pattern (lmap l s) (rmap r g)

instance Functor (Pattern a s) where
  fmap = rmap

patternPrism :: Prism' s a -> Pattern a s s
patternPrism p = withPrism p $ \s g -> Pattern (hush . g) s

pAdd :: WithTmNum t f a => Pattern (f a, f a) (f a) (f a)
pAdd = patternPrism _TmAdd

-- we are making recursive calls, so we want to resize the gen we are passed in
-- are we passed a Term a, or a Term a with type Int?
gAdd :: WithTmNum t f a => Gen (f a) -> Gen (f a)
gAdd g = fmap (review _TmAdd) <$> g' <*> g'
  where
    g' = sized (\s -> resize (s `div` 2) g)

sAdd :: WithTmNum t f a => f a -> Maybe [f a]
sAdd t = do
  (x, y) <- scatter pAdd t
  let zs = fmap (gather pAdd) . shrink $ (x, y)
  return x : y : shrink xs ++ shrink ys ++ zs


-- Gens

-- Type checking and inference

-- TODO some kind of AccValidation would be good here

data TypeError t =
  Unexpected t t

expect :: (Eq t) => t -> t -> Either (TypeError t) ()
expect t1 t2 =
  if t1 == t2
     then return ()
     else Left (Unexpected t1 t2)

{-
checkAdd :: f a -> t -> Maybe (Either (TypeError t) ())
checkAdd t = do
  r <- inferAdd t
  return (expect r (review _TyInt))

inferAdd :: AsTyInt t => (f a -> t -> Either (TypeError t) ()) -> f a -> Maybe (Either (TypeError t) t)
inferAdd check t = do
  (x, y) <- pAdd t
  let r = do
      check x (review _TyInt)
      check y (review _TyInt)
      return (review _TyInt)
  return r
-}

-- Small step semantics

-- looks like the Star instance of proap would work here

pAddIntInt :: (WithTmNum t f a, WithTmInt t f a) => f a -> Maybe (Int, Int)
pAddIntInt t = do
  (x, y) <- scatter pAdd t
  (,) <$> pIntLit x <*> pIntLit y

eAddIntInt :: (WithTmNum t f a, WithTmInt t f a) => f a -> Maybe (f a)
eAddIntInt = fmap (review _TmIntLit . uncurry (+)) . pAddIntInt

-- step should take a step to a term of type Int
-- pure should be a term of type Int
-- we can handle this in the semantics, or we can assume we've done type checking first
-- it would be nice to be able to build ill-typed terms, so that we can test what the type checker does in that case
-- we also want the evaluator to get stuck on ill-typed terms, so it shouldn't move in some cases
-- - will need to check TAPL for the details

-- test p = fmap (gather p) . scatter p
-- proap (scatter pAdd) (gather pInt . uncurry (+)) (test pIntLit) (test pIntLit)
-- proap (scatter pAdd) (gather pAdd) step pure
-- proap (scatter pAdd) (gather pAdd) (test pIntLit) step

pAdd1 :: (WithTmNum t f a) => (f a -> Maybe (f a)) -> f a -> Maybe (f a, f a)
pAdd1 step t = do
  (x, y) <- scatter pAdd t
  (,) <$> step x <*> pure y

eAdd1 :: (WithTmNum t f a) => (f a -> Maybe (f a)) -> f a -> Maybe (f a)
eAdd1 step = fmap (gather pAdd) . pAdd1 step

pAdd2 :: (WithTmNum t f a, WithTmInt t f a) => (f a -> Maybe (f a)) -> f a -> Maybe (f a, f a)
pAdd2 step t = do
  (x, y) <- scatter pAdd t
  (,) <$> fmap (review _TmIntLit) (pIntLit x) <*> step y

eAdd2 :: (WithTmNum t f a, WithTmInt t f a) => (f a -> Maybe (f a)) -> f a -> Maybe (f a)
eAdd2 step = fmap (gather pAdd) . pAdd2 step

{-

(+) :: Int -> Int -> Int

Treat x + y as App (App (+) y) x

How do work that into the story for the semantics?

App (App PAdd (Int x)) (Int y) => Int (x + y)
App (App PAdd x) y => App (App PAdd (step x)) y
App (App PAdd (Int x)) y => App (App Padd (Int x)) (step y)

This allows us to work with partially applied primitive functions and things like sections

Semantics requires
- pattern matching
- gens / shrinks
- a family of helper Term patterns
  - any term
  - any term that takes a step
  - a term of a particular type

We can combine all of these together with asum
- we might need some extra machinery to combine them together to form those families of helper patterns easily

-- context via reader
-- overall check and infer functions via reader

-- need a Maybe in here to reflect the pattern match
checkAdd :: TmAdd term -> type -> Maybe (Either TypeError ())
checkAdd type (add x y) = do
  check x int
  check y int
  expect int type

inferAdd :: TmAdd term -> Maybe (Either TypeError type)
inferAdd (add x y) = do
  check x int
  check y int
  return int

checkVar -- check from context
inferVar -- infer from context

checkApp (App f x) r
  Arr yt zt <- infer f
  xt <- infer x
  expect xt yt
  expect zt r

inferApp (App f x)
  Arr yt zt <- infer f
  xt <- infer x
  expect xt yt
  return zt

instantiate Lam with type to help checking work better?
- will this fall down with more advanced type systems?

-}

-- TmAdd (TmInt x) (TmInt y) => TmInt (x + y)
sAddIntInt :: (WithTmNum t f a, WithTmInt t f a) => f a -> Maybe (f a)
sAddIntInt = undefined

-- do we require that x and y are well-typed here?
-- - does type checking take care of that for us?
-- TmAdd x y => TmAdd (step x) y
sAddStep1 :: f a -> Maybe (f a)
sAddStep1 = undefined

-- TmAdd (Int x) y => TmAdd (Int x) (step y)
sAddStep2 :: f a -> Maybe (f a)
sAddStep2 = undefined

-- TmSub (TmInt x) (TmInt y) => TmInt (x - y)
sSubIntInt :: (WithTmNum t f a, WithTmInt t f a) => f a -> Maybe (f a)
sSubIntInt = undefined

-- TmSub x y => TmSub (step x) y
sSubStep1 :: f a -> Maybe (f a)
sSubStep1 = undefined

-- TmSub (Int x) y => TmSub (Int x) (step y)
sSubStep2 :: f a -> Maybe (f a)
sSubStep2 = undefined

-- TmMul (TmInt x) (TmInt y) => TmInt (x * y)
sMulIntInt :: (WithTmNum t f a, WithTmInt t f a) => f a -> Maybe (f a)
sMulIntInt = undefined

-- TmMul x y => TmMul (step x) y
sMulStep1 :: f a -> Maybe (f a)
sMulStep1 = undefined

-- TmMul (Int x) y => TmMul (Int x) (step y)
sMulStep2 :: f a -> Maybe (f a)
sMulStep2 = undefined

-- TmNeg (TmInt x) => TmInt (-1 * x)
sNegInt :: (WithTmNum t f a, WithTmInt t f a) => f a -> Maybe (f a)
sNegInt = undefined

-- TmNeg x => TmNeg (step x)
sNegStep :: f a -> Maybe (f a)
sNegStep = undefined

-- Big step semantics

-- there's a notion of base types in here
-- there's a notion of values - bStep is the transitive closure of small steps until we hit a value
-- bStep x -> Int i, bStep y -> Int j => TmInt (x + y)
bAdd :: f a -> f a -> Maybe (f a)
bAdd = undefined

-- Syntax

infixl 6 .+.
(.+.) :: WithTmNum t f a => f a -> f a -> f a
(.+.) x y = review _TmAdd (x, y)

infixl 6 .-.
(.-.) :: WithTmNum t f a => f a -> f a -> f a
(.-.) x y = review _TmSub (x, y)

infixl 7 .*.
(.*.) :: WithTmNum t f a => f a -> f a -> f a
(.*.) x y = review _TmMul (x, y)

neg :: WithTmNum t f a => f a -> f a
neg = review _TmNeg

