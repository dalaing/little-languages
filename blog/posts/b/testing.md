---
title: Testing B
published: 2016-06-27 12:00:00+10:00
---

# QuickCheck for *B*

[Previously](semantics.html) we defined the semantics for *B*, and ended up with a set of properties we wanted to check.

If you're not familiar with `QuickCheck`, I have written a little tour of the package [here](../packages/quickcheck.html).
I'd recommend have a read through and then coming back here.

If you are familiar with `QuickCheck` and decide to skip that post, the things you'll need to know for this are:

- we're using shrinking wherever we can (including using `forAllShrink` instead of `forAll`)
- we're using pretty simple size strategies when recursively generating terms, at least for the time being

Now that that's out of the way, let's sink our teeth into writing some `QuickCheck` code for *B*.

## Generators 

We start of with some simple helper functions, to generate each of the terms in *B*.

As usual, the cases for `TmFalse` and `TmTrue` are simple:
```haskell
genTmFalse :: Gen Term
genTmFalse = 
  pure TmFalse
```

```haskell
genTmTrue :: Gen Term
genTmTrue = 
  pure TmTrue
```

We provide a very generic generator for `TmIf`:
```haskell
genTmIf :: Gen Term 
        -> Gen Term 
        -> Gen Term 
        -> Gen Term
genTmIf g1 g2 g3 =
  TmIf <$> g1 <*> g2 <*> g3
```

I find that it's usually good to build up an army of generators, in order to make it easier to think of and add new properties as soon as you think of them.

With these generators in place, we can build a generator for `Term`.

Since `Term` is a recursive data type, we'll have to take some care with `QuickCheck`'s size parameter.

We'll be interpreting the size parameter as an upper bound on the size of a `Term` - with the various caveats and addendums that were mentioned earlier in the `Tree` example -  where `size` is given by:
```haskell
size :: Term 
     -> Int
size TmFalse = 
  1
size TmTrue = 
  1
size (TmIf t1 t2 t3) =
  1 + size t1 + size t2 + size t3
```
which is just counting the number of constructors used to build that particular `Term`.

We end up with:
```haskell
genTerm :: Gen Term
genTerm = 
  sized genTerm'
  
genTerm' :: Int 
         -> Gen Term
genTerm' s = 
    (if s == 0 then [] else nonZeroSizedGens) ++
    zeroSizedGens
  where
    zeroSizedGens = [
        genTmFalse
      , genTmTrue
      ]
    nonZeroSizedGens = 
      let
        s' = s `div` 3
        child = genTerm s'
      in 
        [genTmIf child child child]
```

This is a bit of a mess, but we can clean it up by writing helper functions to do size-aware generation for each constructor of our term that has a recursive component.

In this case, we only need such a helper for 'TmIf'.
```haskell
genTermTmIf :: (Int -> Gen Term)
            -> Int
            -> Maybe (Gen Term)
genTermTmIf _  0 =
  Nothing
genTermTmIf gen s =
  let
    child = gen (s `div` 3)
  in
    Just $ genTmIf child child child
```

The function gets passed a size-aware generator for terms and the size that we're after, and returns the appropriate 'Gen' when the size is larger than 0. 

```haskell
genTerm :: Gen Term
genTerm = sized genTerm'

genTerm' :: Int
         -> Gen Term
genTerm' s =
  oneof $ [
      genTmFalse
    , genTmTrue
    ] ++ mapMaybe (\f -> f genTerm' s) [
      genTermTmIf
    ]
```

## Shrinking functions 

There's nothing to shrink for the `TmFalse` and `TmTrue` cases:
```haskell
shrinkTmFalse :: Term 
              -> Maybe [Term]
shrinkTmFalse TmFalse = 
  Just []
shrinkTmFalse _ =
  Nothing
```

```haskell
shrinkTmTrue :: Term 
             -> Maybe [Term]
shrinkTmTrue TmTrue = 
  Just []
shrinkTmTrue _ =
  Nothing
```

With `TmIf`, we want to return 
    - the immediate sub-terms, and 
    - versions of the `TmIf` term where each of the sub-terms have been shrunk
   
We pass in the combined shrinking function for terms as a function argument, just like we did with the values and small-step rules:
```haskell
shrinkTmIf :: (Term -> [Term]) 
           -> Term 
           -> Maybe [Term]
shrinkTmIf shr (TmIf t1 t2 t3) = Just $
  [t1, t2, t3] ++
  fmap (\t1' -> TmIf t1' t2 t3) (shr t1) ++
  fmap (\t2' -> TmIf t1 t2' t3) (shr t2) ++
  fmap (\t3' -> TmIf t1 t2 t3') (shr t3)
shrinkTmIf _ _ = 
  Nothing 
```
and we combine these shrinking functions together in the usual fashion:
```haskell
shrinkTermRules :: [Term -> Maybe [Term]]
shrinkTermRules = [
   shrinkTmFalse
 , shrinkTmTrue
 , shrinkTmIf shrinkTerm 
 ]

shrinkTerm :: Term
           -> [Term]
shrinkTerm tm =
  fromMaybe [] .
  asum .
  fmap ($ tm) $
  shrinkTermRules
```

Now we just need to wrap that up in a newtype and make an `Arbitrary` instance for it:
```haskell
newtype AnyTerm =
  AnyTerm { getAnyTerm :: Term }
  deriving (Eq, Show)

instance Arbitrary AnyTerm where
  arbitrary =
    fmap AnyTerm genTerm
  shrink =
    fmap AnyTerm . shrinkTerm . getAnyTerm
```

## Checking our work

Before we get too excited, it's always a good idea to spend a little time in GHCi and check our working.

Generating terms seems to work:
```haskell
> sample genTerm
TmTrue
TmTrue
TmTrue
TmTrue
TmTrue
TmIf (TmIf TmFalse (TmIf TmTrue TmTrue TmFalse) TmFalse) TmFalse TmTrue
TmFalse
TmTrue
TmTrue
TmIf TmTrue (TmIf TmFalse TmFalse TmTrue) TmTrue
TmTrue
```

And so does shrinking terms (with some liberties taken around the formatting):
```haskell
> shrinkTerm TmTrue
[]
> shrinkTerm $ TmIf TmTrue (TmIf TmFalse TmFalse TmTrue) TmTrue
[ TmTrue
, TmIf TmFalse TmFalse TmTrue
, TmTrue
, TmIf TmTrue TmFalse TmTrue
, TmIf TmTrue TmFalse TmTrue
, TmIf TmTrue TmTrue TmTrue
]
```

Now we're ready to do some stuff!

# Properties

We're now going to convert the various properties that we want to hold into `QuickCheck`.

- Every value is a normal form
```haskell
propValueNormal :: AnyTerm -> Property
propValueNormal (AnyTerm tm) =
  isValue tm ==> isNormalForm tm
```

- *B* has no stuck terms
```haskell
propNormalValue :: AnyTerm -> Property
propNormalValue (AnyTerm tm) =
  isNormalForm tm ==> isValue tm
```

- *B* is determinate

    Which means that for a given term, the output of all of the steps that apply to that term should agree.
```haskell
propSmallDeterminate :: AnyTerm -> Property
propSmallDeterminate (AnyTerm tm )=
  canStep tm ==>
    let
      distinctResults =
        length .
        group .
        mapMaybe ($ tm) $
        smallStepRules
    in
      distinctResults === 1
```

- *B* is normalizing

    Which we demonstrate by showing that the `smallStep` function always reduces the size of a term. 
```haskell
propSmallShrinks :: AnyTerm -> Bool
propSmallShrinks (AnyTerm tm) =
  case smallStep tm of
    Nothing -> True
    Just tm' -> size tm' < size tm
```

We also throw in some extra tests, to make sure that we're not going to far off track.

We check that for each term, there is exactly one rule from the value or small step rules that holds:
```haskell
propSmallUnique :: AnyTerm -> Property
propSmallUnique (AnyTerm tm) =
  let
    matches =
      length .
      mapMaybe ($ tm) $
      valueRules ++ smallStepRules
  in
    matches === 1
```
which will confirm that the rules are distinct and exhaustive.

Something a little weaker than this is implied by the first few rules, so this is mostly redundant.
I'm throwing it in here as an early-warning canary, but I'll take it out if it gets in my way.
It's already paid for itself as I've experimented with various things.

We also check that the small step evaluation function matches the big step evaluation function:
```haskell
propSmallBig :: AnyTerm -> Property
propSmallBig (AnyTerm tm) =
  Term.Eval.SmallStep.eval tm === Term.Eval.BigStep.eval tm
```

This also acts as a sanity check, and has also helped me to spot bugs sooner than I would have otherwise.

I could have removed a few of the other properties once `propSmallUnique` was written, but I didn't.
I like to have `QuickCheck` tests written out explicitly for the properties that I want to hold.
I'm fine with having a bit of redundancy between the properties if those properties are well known in the domain.

# Syntax for our semantics

The above tests all pass and so we should be getting confident in our little language - so let's take it our for a spin.

A great way to play with a language is through a read-eval-print-loop (REPL), which is what we're going to look at next.
This will mean we'll have to work out how to parse and print expressions in our language, which will provide some additional entertainment.

[Read on!](text.html)

# Exercises for the adventurous
- Add some more properties that are reasonable for *B*.
- Are any of the properties redundant? Think about how we might work that out.  Would you prefer a minimum set of properties, or an overlapping set of properties that map neatly to the concepts from PLT? 
- Investigate small check.  What is the maximum depth term we need to be confident in all of our properties? 
- Write a REPL for *B*.
