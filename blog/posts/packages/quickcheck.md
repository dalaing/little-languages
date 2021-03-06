---
title: An introduction to `QuickCheck`
published: 2016-06-28 12:00:00+10:00
---

# QuickCheck

`QuickCheck` is a Haskell package for use with property-based testing.

That means we can write properties like
```haskell
sort_idempotent_prop :: [A] -> Bool
sort_idempotent_prop xs =
  sort xs == sort (sort xs)
```
to express that sorting a list once and sorting a list twice should produce the same results and `QuickCheck` will try to find out if the property holds or not.

`QuickCheck` does this by testing whether the property holds for all kinds of lists of `A` - empty lists, small lists, big lists - and if the property fails to hold it will look for a minimal test case that cause the property not to hold. 
This can be a huge help.

We'll begin this section with a lightning tour of `QuickCheck`, since we're going to be using it to check that our semantics are sensible.

## Gen

At the heart of `QuickCheck` is the `Gen` monad.

We can play around with `Gen` in GHCi to get a feel for it.
The `sample` function will come in handy for this.
```haskell
> :t sample
sample :: Show a => Gen a -> IO ()
```
It runs the given `Gen` a number of times and prints the results.

The `pure` / `return` function from `Applicative` / `Monad` creates the most boring of `Gen`s:
```haskell
> sample $ pure (2 :: Int)
2
2
2
2
2
2
...
```

We can choose from a list of elements with `elements`:
```haskell
> :t elements
elements :: [a] -> Gen a
> sample $ elements [2 :: Int, 3]
3
3
2
3
3
3
...
```
and we can chose from a list of `Gen`s with `oneof`:
```haskell
> :t oneof
oneof :: [Gen a] -> Gen a
> sample $ oneof [pure (2 :: Int), pure 3]
3
2
2
3
2
...
```

We can take the `Functor` instance for a run:
```haskell
> sample $ (* 10) <$> elements [1 :: Int .. 10]
80
30
30
50
100
...
```
and we can extend that to the `Applicative` instance:
```haskell
> sample $ (,) <$> elements [1 :: Int .. 10] <*> elements ['a'..'e']
(5,'d')
(6,'b')
(1,'d')
(4,'d')
(4,'e')
...
```

## Arbitrary

`QuickCheck` also comes with an `Arbitrary` typeclass.

```haskell
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]
```

This lets us package up a canonical `Gen` for our own types, and also provides a shrinking function which is used when a `Property` fails and `QuickCheck` is searching for counter-examples.

There are lots of instances of `Arbitary` provided already, and it's fairly easy to write your own.

Let us have a look at what the `String` instance does:
```haskell
> sample $ (arbitrary :: Gen String)
"\EM\203"
""
"\RSDi8G$"
"a"
"T\176W("
...

> shrink "test"
["","st","te","est","tst","tet","tes","aest","best","cest","tast","tbst","tcst","teat","tebt","tect","tesa","tesb","tesc"]
```
and what the `Int` instance does:
```haskell
> sample $ (arbitrary :: Gen Int)
-1
-6
6
-9
1
...

> shrink (10 :: Int)
[0,5,8,9]
```

`QuickCheck` will try the outputs of `shrink` in order, and we can see that the built-in instances have outputs ordered by most agressive shrinking to least aggressive shrinking.

Writing our own instances is mostly straightforward, although there are a few options and tricks to be aware of.

If we have a toy data structure:
```haskell
data Toy = ToyIB Int Bool
         | ToyS String
         deriving (Eq, Show)
```
and want to write an `Arbitrary` instance for it:
```haskell
instance Arbitrary Toy where
```
the `arbitrary` function is just a matter of using `oneof` and the `Applicative` interface for `Gen`:
```haskell
  arbitrary = 
    oneof [
        ToyIB <$> arbitrary <*> aribtrary
      , ToyS <$> arbitrary
      ]
```
although we can change the relative frequency of the constructors using `frequency`:
```haskell
  arbitrary = 
    frequency [
        (2, ToyIB <$> arbitrary <*> aribtrary)
      , (1, ToyS <$> arbitrary)
      ]
```

The `shrink` function requires a little more care.
We could do the shrinking in the list monad, but that doesn't do what we want when we have multiple things to shrink at once.

If we try to do
```haskell
  shrink (ToyIB i b) = do
    i' <- shrink i
    b' <- shrink b
    return (ToyIB i' b')
```
the shrinking will stop once we exhaust the possible shrinks of `i`.

Instead, we should make use of the instances of `Arbitrary` for tuples, which do the right thing:
```haskell
  shrink (ToyIB i b) = do
    (i', b') <- shrink (i, b)
    return (ToyIB i' b')
  shrink (ToyS s) = 
    ToyS <$> shrink s
```

We could unpack that further with
```haskell
  shrink (ToyIB i b) =
    fmap (\i' -> ToyIB i' b) (shrink i) ++
    fmap (\b' -> ToyIB i b') (shrink b)
  shrink (ToyS s) = 
    ToyS <$> shrink s
```

The pitfalls with shrinking are called out in more detail in the documentation for the `Arbitrary` typeclass, which also mentions the support for shrinking via generics.

The other thing to watch out for comes about when you are generating a recursive data structure.

If we have a `Tree` data type:
```haskell
data Tree a = Branch (Tree a) (Tree a)
            | Leaf a
            deriving (Eq, Show)
```
and want to write an `Arbitrary` instance for it:
```haskell
instance Arbitrary a => Arbitrary (Toy a) where
```
then a straightforward attempt at `arbitrary`:
```haskell
  arbitrary = 
    oneof [
      Branch <$> arbitrary <*> arbitrary
    , Leaf <$> arbitrary
    ]
```
will hit a snag, in that it will occasionally produce trees of infinite size.

Thankfully there is a way around this.
The generators in `QuickCheck` make use of an implicit size parameter, where the meaning of the size parameter is decided by the implementor of the generator.

We can change the size with `resize`:
```haskell
> sample $ (arbitrary :: Gen Int)
-1
-6
6
-9
1
...
> sample $ resize 1000 (arbitrary :: Gen Int)

433
-662
972
233
17
...
```
and we can use `size` to get access to that parameter.

This is just what we need, since we can choose to interpret the (finite) size parameter as an upper bound on the size of our tree, and we'll end up with finite sized trees.

Here's a simple approach:
```haskell
  arbitrary = sized genTree
    where
      genTree 0 = 
        Leaf <$> arbitrary
      genTree s = 
        let 
          s2 = s `div` 2
          genSubTree = genTree s2
        in
          oneof [
            Leaf <$> arbitrary
          , Branch <$> genSubTree <*> genSubTree
          ]
```

Note however that the size of the tree is actually bounded by the size parameter plus one.

Also note that none of the trees bounded by the size 10 are going to have subtrees with sizes 8 and 2.
We'll still get those trees when the size parameter is 16 or above, but it does mean that our interpretation is a bit wobbly.

We can do much better on this front.
I recommend Brent Yorgey's posts [here](https://byorgey.wordpress.com/2013/04/25/random-binary-trees-with-a-size-limited-critical-boltzmann-sampler-2/) and [here](https://byorgey.wordpress.com/2016/03/23/boltzmann-sampling-for-generic-arbitrary-instances/) if you're interested, and the code [here](https://github.com/Lysxia/generic-random).
I have some other things up my sleeve which I hope to cover later, which is why I'm leaving that alone for now.

Given all of that, the implementation of `shrink` is something of a relief:
```haskell
  shrink (Branch t1 t2) = 
    t1 :: t2 :: fmap (\(b1, b2) -> Branch b1 b2) (shrink (t1, t2))
  shrink (Leaf l) = 
    Leaf <$> shrink l
```

## Properties

A `Property` is something we want to test.

There is a `Testable` typeclass that converts things to `Property`:
```haskell
instance Testable Bool
instance Testable Property
instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)
```
and so we can do either:
```haskell
sort_idempotent_prop :: [Int] -> Bool
sort_idempotent_prop xs =
  sort xs == sort (sort xs)
```
or:
```haskell
sort_idempotent_prop :: [Int] -> Property
sort_idempotent_prop xs =
  property $ sort xs == sort (sort xs)
```
because we have an `Arbitrary` instance for `[Int]`.

We can test these using `quickCheck`:
```haskell
> quickCheck sort_idempotent
+++ OK, passed 100 tests.
```

If we're putting `Property` in the signature, we might as well use some of it's features.

Let's look at a property that fails:
```haskell
reverse_idempotent_prop :: [Int] -> Property
reverse_idempotent_prop xs =
  property $ reverse xs == reverse (reverse xs)
  
> quickCheck reverse_idempotent_prop
*** Failed! Falsifiable (after 7 tests and 5 shrinks):
[0,1]
```

We can use `===` to print counterexamples when a test fails:
```haskell
reverse_idempotent_prop :: [Int] -> Property
reverse_idempotent_prop xs =
  reverse xs === reverse (reverse xs)
  
> quickCheck reverse_idempotent_prop
*** Failed! Falsifiable (after 5 tests and 1 shrink):
[0,1]
[1,0] /= [0,1]
```
which looks simple here, but can be a huge time-saver with more complex properties.

We have a couple of options for using `Gen`s to produce a `Property`.

Let's say I have the following:
```haskell
genLeftSkewedTree    :: Arbitrary a => Gen (Tree a)
shrinkLeftSkewedTree :: Tree a -> [Tree a]
```

We can wrap this up in a newtype:
```haskell
newtype LeftSkewedTree a = 
  LeftSkewedTree { getLSTree :: Tree a}
  deriving (Eq, Show)
  
instance Arbitrary a => Arbitrary (LeftSkewedTree a) where
  arbitrary = LeftSkewedTree <$> genLeftSkewedTree
  shrink = fmap LeftSkewedTree . shrinkLeftSkewedTree . getLSTree
``` 
and use it in a property like this:
```haskell
myProp :: LeftSkewedTree Int -> Bool
myProp lst = ...
```

We can also just use it directly, via `forAll` or `forAllShrink`:
```haskell
myProp' :: Tree Int -> Bool
myProp' t = ...

-- if we care about shrinking
myProp1 :: Property
myProp1 = 
  forAllShrink genLeftSkewedTree shrinkLeftSkewedTree myProp'

-- if we don't care about shrinking
myProp2 :: Property
myProp2 = 
  forAll genLeftSkewedTree myProp'
```

I tend to use the newtype approach most of the time.
If something is common and I'm sure it's sticking around, I'll tend to accumulate functionality around the generators, and the newtype tends to help keep that in order.
It also makes the properties easier to read.

I tend to use `forAll` and `forAllShrink` if I need a one-off combination of `Gen`s for a property, if I can't easily use a newtype for some reason, or if I'm prototyping something and aren't sure if I'm going to need the `Gen` in that form when I'm done.

Working with a `Property` also means we can also use `==>` to filter out invalid inputs.

We can take this failing property:
```haskell
reverse_head :: [Int] -> Property
reverse_head xs =
  head xs === last (reverse xs)

> quickCheck reverse_head
*** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
[]
Exception thrown while printing test case: 'Prelude.head: empty list'
```
and fix it up:
```haskell
reverse_head :: [Int] -> Property
reverse_head xs =
  (not . null $ xs) ==>
    head xs === last (reverse xs)

> quickCheck reverse_head
+++ OK, passed 100 tests.
```

If the filtering is to aggressive we may not be able to generate enough data to fulfill the test.

```haskell
propReverseSingle :: [Int]
                  -> Property
propReverseSingle xs =
  length xs == 1 ==> 
    xs == reverse xs

> quickCheck propReverseSingle
*** Gave up! Passed only 43 tests.
```

I tend to use `==>` when I'm certain that I'm dealing with an edge case that makes up a small percentage of the test data (and I usually check that with `classify`, `collect` or `cover`).

If that's not the case I tend to expand my library of `Gen` related helpers.

```haskell
genSingletonList :: Arbitrary a
                 => Gen [a]
genSingletonList = do
  x <- arbitrary
  return [x]

shrinkSingletonList :: Arbitrary a
                    => [a]
                   -> [[a]]
shrinkSingletonList [x] =
  fmap pure (shrink x)
shrinkSingletonList _ =
  []
  
newtype SingletonList a = SingletonList {
    getSingletonList :: [a]
  } deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (SingletonList a) where
  arbitrary =
    fmap SingletonList genSingletonList
  shrink =
    fmap SingletonList .
    shrinkSingletonList .
    getSingletonList

propReverseSingle :: SingletonList Int
                  -> Bool
propReverseSingle (SingletonList xs) =
  xs == reverse xs

> quickCheck propReverseSingle
+++ OK, passed 100 tests.
```
