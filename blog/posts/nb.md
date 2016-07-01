---
title: NB - our first combination of types
published: 2016-07-01 12:00:00+10:00
---

Previously we've looked at a couple of different little languages - [*B*](b.html), [*N*](n.html) and [*I*](i.html).

So far we've been able to get away with reusing the tests suites and REPL, and things have been going smoothly.

# Combining *N* and *B*

Let's see what happens if we put together a project with a term type that combines the terms from *N* and *B*, in order to form the language *NB*:

The code for *NB* is available [here](https://github.com/dalaing/little-languages/tree/master/code/nb).

$$
\begin{aligned}
t =& \ \text{O} \\
  |& \ \text{S t} \\
  |& \ \text{pred t} \\
  |& \ \text{true} \\
  |& \ \text{false}  \\
  |& \ \text{if $t$ then $t$ else $t$}
\end{aligned}
$$

and in Haskell:
```haskell
data Term =
    TmZero
  | TmSucc Term
  | TmPred Term
  | TmFalse
  | TmTrue
  | TmIf Term Term Term
  deriving (Eq, Ord, Show)
```
and copy the various rules across.

We should still end up with an evaluator, parsing and pretty printing functions and support for test data generation - albeit with a long list of rules contributing to each of them.

One thing that has changed is that we have some test failures:
```haskell
term
  eval
    every normal form is a value:    FAIL
      *** Failed! Falsifiable (after 9 tests):
      AnyTerm {getAnyTerm = TmPred TmFalse}
    small step rules are unique:     FAIL
      *** Failed! Falsifiable (after 4 tests):
      AnyTerm {getAnyTerm = TmPred TmTrue}
      0 /= 1
    small step and big step agree:   FAIL
      *** Failed! Falsifiable (after 24 tests and 3 shrinks):
      AnyTerm {getAnyTerm = TmIf TmTrue (TmPred TmTrue) TmZero}
      TmPred TmTrue /= TmIf TmTrue (TmPred TmTrue) TmZero
```

They first two arise from the fact that we have some terms for which the step function is not defined - ie normal forms - other than the values.

This includes things like `TmSucc TmFalse` and `TmIf TmZero TmFalse TmTrue`.
It happens that these look like nonsensical terms, so it would be nice to rule those out.

# Typing rules

This is where type systems enter the story.

We'll be looking at the type systems through a system of rules similar to what we had when we looked at small-step semantics.

Where we used to use

$$
\prftree[r]{Step-Rule}{\text{before} \longrightarrow \text{after}}
$$

to represent the rule that makes term $\text{before}$ take a single step to become the term $\text{after}$, we will use

$$
\prftree[r]{Type-Rule}{\text{t} \colon \text{T}}
$$

to represent the rule that associates the term $\text{t}$ with the type $\text{T}$.

If these rules can be used to associated a type with a term, the term is said to be well-typed.
If there is no rule that applies to a term, it is ill-typed.

In the material that I've seen, the typing rules are usually presented but there is little formalisation about what to do with ill-typed terms.

We're going to do the same here, at least for now, although it's something we'll need to consider when we're implementing the type system.

The two main operations that we will be performing with these rules are type checking - where we are checking an assertion that a particular term has a particular type - and type inference - where we are given a term and try to work out what type it has.

If we can do type inference, then type checking is trivial: we just need to check the inferred type for the term against the claimed typed of the term.

Type inference isn't possibly in all languages, so we'll end up with a number of trade-offs involving how much information we have to provide to help the inference process along.

There is also another approach worth knowing about - where the rules are split into checking rules and inference rules - known as bidirectional typing.
I hope to return to this later on.

# Typing for NB

In order to talk about the types of *NB*, we should define them.

There's only two of them, and they're both very simple.

This will be our notation:

$$
\begin{aligned}
T =& \ \text{Nat} \\
  |& \ \text{Bool}
\end{aligned}
$$

and the translation to Haskell is easy:

```haskell
data Type =
    TyNat
  | TyBool
  deriving (Eq, Ord, Show)
```

The rules start out very simple.

The values which don't involve other terms have typing rules with no assumptions.

The term `O` has type `Nat`:

$$
\prftree[r]{T-Zero}
{\text{O} \colon \text{Nat}}
$$

and the terms `true` and `false` have type `Bool`:

$$
\prftree[r]{T-True}
{\text{true} \colon \text{Bool}}
$$

$$
\prftree[r]{T-False}
{\text{false} \colon \text{Bool}}
$$

The successor and predecessor terms are a little more complicated.
They each have type `Nat` if their arguments have type `Nat`:

$$
\prftree[r]{T-Succ}
{\text{t} \colon \text{Nat}}
{\text{S t} \colon \text{Nat}}
$$

$$
\prftree[r]{T-Pred}
{\text{t} \colon \text{Nat}}
{\text{pred t} \colon \text{Nat}}
$$

The `if` terms are a bit more frisky.

If the first argument has type `Bool`, and the second and third arguments have the same type `T`, then the whole `if` term is of type `T`:

$$
\prftree[r]{T-If}
{t_1 \colon \text{Bool}}
{t_2 \colon \text{T}}
{t_3 \colon \text{T}}
{\text{if $t_1$ then $t_2$ else $t_3$} \colon \text{T}}
$$

This gives rise to two interesting situations.

We can use a term from *B* to create a term from *N*:

```
if true then O else S O
```

and we can create a term that is ill-typed but will still evaluate under the combination of small-step evaluation rules: 

```
if true then O else false
```

We'll have a closer look at that when we cover the properties that we want to hold for our type systems.

# Typing in Haskell

The general approach that we're going to take to set up this rule system in Haskell is similar to what we have done so far.

We're going to create a set of rules that will be combined to create a type inference function.
Since some of those rules are inductively defined, they will be given the type inference function as an input.
This is fine, because of Haskell's laziness.

While we ignored the ill-typed terms when specifying the typing rules, they're the first thing we need to deal with when writing code for our type system.

## Type errors

We'll be doing our error handling with `MonadError` from [Control.Monad.Except](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html) from the `mtl` package.

The `MonadError` class takes two type parameters - the error type and the monad type.

If we have some error type:
```haskell
data MyErrorType = 
    UnsupportedUniverse
  | UnknownPotato
```
and we have a function with `MonadError MyErrorType m` in the context:
```haskell
myErrorFunction :: MonadError MyErrorType m 
                => m Int
myErrorFunction =
```
we can raise errors in that context with `throwError`:
```haskell
  if (1 == 2)
  then throwError UnsupportedUniverse
  else return 3
```

We'll often write little helper functions to check our error conditions:
```haskell
checkUniverse :: MonadError MyErrorType m
              => m ()
checkUniverse =
  when (1 == 2) $
    throwError UnsupportedUniverse
```
in which case our old function would turn into
```haskell
myErrorFunction :: MonadError MyErrorType m 
                => m Int
myErrorFunction = do
  checkUniverse
  return 3
```

Our _actual_ error type will look like this: 
```haskell
data TypeError =
    Unexpected Type Type
  | ExpectedEq Type Type
  | NoMatchingTypeRule
  deriving (Eq, Ord, Show)
```

We're including `NoMatchingTypeRule` here so that we have a value we can return if no rule applies to a term.
We _could_ have left that out and used `Maybe TypeErrro` in its place but we didn't.
This is mostly to avoid a form of Boolean blindness.
We don't want people (i.e. me, later on) thinking that we are missing a `TypeError`, so we encode the reason or reasons that lead to us not having more information available.

Later on we'll test that type inference never results in `NoMatchingTypeRule`, and then we'll breathe a sigh of relief.

Most of the action is centered around two helper functions which check for our error conditions.

The first is used to check that we're dealing with the type that we expect:
```haskell
expect :: MonadError TypeError m
       => Type
       -> Type
       -> m ()
expect actual expected =
  unless (actual == expected) $
    throwError $ Unexpected actual expected
```
and the other is used to check that two types are equal:
```haskell
expectEq :: MonadError TypeError m
         => Type
         -> Type
         -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwError $ ExpectedEq ty1 ty2
```

## Typing rules

We're going to bring the type checking rules across to Haskell, using `Maybe` to determine which rules apply and using `MonadError TypeError m` to do our error handling.
We could be doing all sorts of things with `m` later on, so we leave it generic for now.

The first few rules don't have any need for any error handling, so we relax the constraint to `Monad m`:

$$
\prftree[r]{T-Zero}
{\text{O} \colon \text{Nat}}
$$

```haskell
inferTmZero :: Monad m
            => Term
            -> Maybe (m Type)
inferTmZero TmZero = Just $
  return TyNat
inferTmZero _ =
  Nothing
```

$$
\prftree[r]{T-False}
{\text{false} \colon \text{Bool}}
$$

```haskell
inferTmFalse :: Monad m
             => Term
             -> Maybe (m Type)
inferTmFalse TmFalse = Just $
  return TyBool
inferTmFalse _ =
  Nothing
```

$$
\prftree[r]{T-True}
{\text{true} \colon \text{Bool}}
$$

```haskell
inferTmTrue :: Monad m
            => Term
            -> Maybe (m Type)
inferTmTrue TmTrue = Just $
  return TyBool
inferTmTrue _ =
  Nothing
```

Once we start working with terms that contain other terms, we need to pass in the combined inference rule and use that to check the sub-terms have the expected type:

$$
\prftree[r]{T-Succ}
{\text{t} \colon \text{Nat}}
{\text{S t} \colon \text{Nat}}
$$

```haskell
inferTmSucc :: MonadError TypeError m
            => (Term -> m Type)
            -> Term
            -> Maybe (m Type)
inferTmSucc infer (TmSucc tm) = Just $ do
  ty <- infer tm
  expect ty TyNat
  return TyNat
inferTmSucc _ _ =
  Nothing
```

$$
\prftree[r]{T-Pred}
{\text{t} \colon \text{Nat}}
{\text{pred t} \colon \text{Nat}}
$$

```haskell
inferTmPred :: MonadError TypeError m
            => (Term -> m Type)
            -> Term
            -> Maybe (m Type)
inferTmPred infer (TmPred tm) = Just $ do
  ty <- infer tm
  expect ty TyNat
  return TyNat
inferTmPred _ _ =
  Nothing
```

Finally, when inferring the type of an `if` expressions, we need to check that the test sub-term has type `Bool` and that the two branches have the same type:

$$
\prftree[r]{T-If}
{t_1 \colon \text{Bool}}
{t_2 \colon \text{T}}
{t_3 \colon \text{T}}
{\text{if $t_1$ then $t_2$ else $t_3$} \colon \text{T}}
$$

```haskell
inferTmIf :: MonadError TypeError m
          => (Term -> m Type)
          -> Term
          -> Maybe (m Type)
inferTmIf infer (TmIf tm1 tm2 tm3) = Just $ do
  ty1 <- infer tm1
  expect ty1 TyBool
  ty2 <- infer tm2
  ty3 <- infer tm3
  expectEq ty2 ty3
  return ty3
inferTmIf _ _ =
  Nothing
```

Now we pull our usual stunt.

We build the list of rules:
```haskell
inferTermRules :: MonadError TypeError m
               => [Term -> Maybe (m Type)]
inferTermRules =
  [ inferTmZero
  , inferTmSucc inferTerm
  , inferTmPred inferTerm
  , inferTmFalse
  , inferTmTrue
  , inferTmIf inferTerm
  ]
```
and use `Alternative` to combine those rules:
```haskell
inferTerm :: MonadError TypeError m
          => Term
          -> m Type
inferTerm tm =
  fromMaybe (throwError NoMatchingTypeRule) .
  asum .
  fmap ($ tm) $
  inferTermRules
```

We're still not running with a concrete `Monad`, but we can deal with that using `runExcept`.

We'll give a different name here, because `runInfer` is likely to change as we push on with this series:
```haskell
runInfer :: Except e a
         -> Either e a
runInfer = runExcept
````

We can now test our new function out:
```haskell
> runInfer . inferTerm $ TmSucc (TmSucc (TmPred TmZero))
Right TyNat

> runInfer . inferTerm $ TmIf TmZero TmFalse TmTrue
Left (Unexpected TyNat TyBool)
```

# Adding `isZero`

At the moment we can come up with terms that will turn a `Bool` into `Nat`, like we do with `TmIf TmTrue TmZero (TmSucc TmZero)`.

It would be nice to be able to turn a `Nat` into a `Bool`.
We're about to take a look at the properties of type systems, so anything we can add that will exercise the type system a little more will help us iron out any kinks.

With that in mind, we add another term to our language:

$$
\begin{aligned}
t =& \ \ldots \\
  |& \ \text{isZero t} \\
   & \ \ldots
\end{aligned}
$$

```haskell
data Term =
  ...
  | TmIsZero Term
  ...
  deriving (Eq, Ord, Show)
```

This also brings us up to parity with one of the example languages in TAPL.

The semantics of `isZero` are described by three rules.

There are two reduction rules - one for the case where the argument is zero:

$$
\prftree[r]{E-IsZeroZero}
{\text{isZero O} \longrightarrow \text{true}}
$$

```haskell
eIsZeroZero :: Term
            -> Maybe Term
eIsZeroZero (TmIsZero TmZero) =
  Just TmTrue
eIsZeroZero _ =
  Nothing
```

and one for the case where the argument is the successor of something:

$$
\prftree[r]{E-IsZeroSucc}
{\text{isZero (S $t_1$)} \longrightarrow \text{false}}
$$

```haskell
eIsZeroSucc :: Term
            -> Maybe Term
eIsZeroSucc (TmIsZero (TmSucc tm)) =
  TmFalse <$ value tm
eIsZeroSucc _ =
  Nothing
```

There is one congruence rule, which at this point should be fairly predictable:

$$
\prftree[r]{E-IsZeroSucc}
{t_1 \longrightarrow t_2}
{\text{isZero $t_1$} \longrightarrow \text{isZero $t_2$}}
$$

```haskell
eIsZero :: (Term -> Maybe Term)
        -> Term
        -> Maybe Term
eIsZero step (TmIsZero tm) = do
  tm' <- step tm
  return $ TmIsZero tm'
eIsZero _ _ =
  Nothing
```

The typing rule ensures that is all we have to worry about:

$$
\prftree[r]{T-IsZero}
{t \colon \text{Nat}}
{\text{isZero t} \colon \text{Bool}}
$$

```haskell
inferTmIsZero :: MonadError TypeError m
              => (Term -> m Type)
              -> Term
              -> Maybe (m Type)
inferTmIsZero infer (TmIsZero tm) = Just $ do
  ty <- infer tm
  expect ty TyNat
  return TyBool
inferTmIsZero _ _ =
  Nothing
```

There's nothing new to the parsing and pretty printing for `isZero`, although it could be a nice typing exercise if you wanted to try adding it yourself.

# Updating the REPL

Now that we have the ability to type-check expressions, we're going to update the REPL.

It used to be that when you entered an expression into the REPL, it showed the term before and after the evaluation step in the result:
```haskell
> if isZero O then O else S O
if isZero O then O else S O ==> O
```

We're going to change that so that it shows the result of evaluation the term along with the type of the term:
```haskell
> if isZero O then O else S O
O : Nat
```
or an error message if the type-checking failed:
```haskell
> if O then O else S O
Unexpected type:
  actual:   Nat
  expected: Bool
```

That's not super helpful, given that there are three terms in that expression with type `Nat`.
We'll improve that soon.

For now, we need to add pretty printing for types and type errors so that we can make this update to the REPL happen.

## Pretty printing types

The first thing we'll do is write a pretty printer for types.

We've got really simple types at the moment, but we package them pretty printing into rules nonetheless:

```haskell
prettyTyNat :: Type
            -> Maybe Doc
prettyTyNat TyNat =
  Just $ reservedConstructor "Nat"
prettyTyNat _ =
  Nothing

prettyTyBool :: Type
             -> Maybe Doc
prettyTyBool TyBool =
  Just $ reservedConstructor "Bool"
prettyTyBool _ =
  Nothing
```
which we combine in the same way as our other pretty printing rules into:
```haskell
prettyType :: Type 
           -> Doc
prettyType ty = 
  ...
```

## Pretty printing errors

With pretty printers for the types, we can go on to write pretty printers for the type errors.

We can do something relatively simple here:
```haskell
prettyTeUnexpected :: TypeError
                   -> Maybe Doc
prettyTeUnexpected (Unexpected ac ex) =
  Just . hang 2 $
    text "Unexpected type:" PP.<$>
    text "actual:" <+> prettyType ac PP.<$>
    text "expected:" <+> prettyType ex
prettyTeUnexpected _ =
  Nothing
```
which will give us output that looks like this:
```haskell
Unexpected type:
  actual: Nat
  expected: Bool
```

We're really after something like this:
```haskell
Unexpected type:
  actual:   Nat
  expected: Bool
```

Why? 
Because if I'm going to use a pretty printing library, I want to spend a little time to try to make things pretty!

We'll put together a little helper function to take care of this.

The idea is that we have a list of pairs, where the first element of the pair is a label and the second element of the pair is the `Doc` we want to label.
This will use `fill`, which adds whitespace on the right-hand side of some text until it has a certain length, and `vcat`, which vertically concatenates a list of documents.

```haskell
tabulate :: [(String, Doc)] -> Doc
tabulate xs =
    vcat .
    fmap pad $
    xs
  where
    maxLength =
      maximum .
      fmap (length . fst) $
      xs
    pad (label, doc) =
      fill maxLength (text label) <+> doc
```

We find the maximum length of all of the labels, and then use fill to pad out the label before appending the corresponding `Doc`.
After that we use `vcat` to turn the whole thing into a single `Doc`.

Now we can have another go at our rules:
```haskell
prettyTeUnexpected :: TypeError
                   -> Maybe Doc
prettyTeUnexpected (Unexpected ac ex) =
  Just . hang 2 $
    text "Unexpected type:" PP.<$>
    tabulate [
        ("actual:",   prettyType ac)
      , ("expected:", prettyType ex)
      ]
prettyTeUnexpected _ =
  Nothing
```

```haskell
prettyTeExpectedEq :: TypeError
                   -> Maybe Doc
prettyTeExpectedEq (ExpectedEq t1 t2) =
  Just . hang 2 $
    text "Expected these types to be equal:" PP.<$>
    tabulate [
        ("type 1:", prettyType t1)
      , ("type 2:", prettyType t2)
      ]
prettyTeExpectedEq _ =
  Nothing
```

```haskell
prettyTeNoMatchingTypeRule :: TypeError
                    -> Maybe Doc
prettyTeNoMatchingTypeRule NoMatchingTypeRule =
  Just $ text "No matching type rule"
prettyTeNoMatchingTypeRule _ =
  Nothing
```

which are combined to form:

```haskell
prettyTypeError :: TypeError
                -> Doc
prettyTypeError te = 
  ...
```

## The change to the REPL

The main loop of the REPL will remain unchanged, but the function at the heart of it will.

Previously we had this:
```haskell
parseAndEval :: String
             -> Doc
parseAndEval s =
  case parseFromString parseTerm s of
    Left d -> 
      d
    Right tm -> 
      prettyTerm tm <+> text "==>" <+> prettyTerm (eval tm)
```
and now we want to type-check the term after we have parsed it but before we evaluate it.

Between `inferTerm`, `prettyTypeError` and `prettyType`, we have the pieces we need to make this happen:
```haskell
parseAndEval :: String
             -> Doc
parseAndEval s =
  case parseFromString parseTerm s of
    Left d -> 
      d
    Right tm -> 
      case runInfer . inferTerm $ tm of
        Left e -> 
          prettyTypeError e
        Right ty ->
          prettyTerm (eval tm) <+> text ":" <+> prettyType ty
```

# Properties of type systems

The main property we are after here is type safety.

A lot of thought went into the definition that we use for type safety, which is:

> Well typed terms do not get stuck

This is usually demonstrated by showing that two other properties hold: progress and preservation.

- Progress

    Every well-typed term is either a value, or can take a small step.
    
- Preservation

    For every well-typed term that can take a small step, the type of the term is the same before and after the step.

This is enough to show that well-typed terms do not get stuck.

Imagine we have an arbitrary well-typed term.

If the term is a value, then it doesn't get stuck.

If it isn't in a value, then by the progress property we know it can take a step.

The result of taking a step will be another well-typed term, thanks to the preservation property, and so we're back where we started except that we're one step further along in the evaluation process.

They are a simple pair of properties, but they relate the type information and the semantics in such a way that we we can rule out programs that would get stuck without having to run them to find that out.

The type system is conservative.
This means that it will err on rejecting some programs that would not get stuck in order to guarantee that it will never accept any program that does get stuck.

One of the casualties is:
```
if true then O else false
```

We pour out some of our drink for that term, and then we move on.

# Testing the type system

In order to test the above properties we're going to need to be able to generate well-typed terms.

If we think back to the start of this post, we also seem to need well-typed terms to make some of our existing tests pass.

We don't have small step rules for the ill-typed terms, and so we need to update the properties that state:

- all terms which are normal forms are also values
- all terms have exactly one small-step rule that applies to them

to apply only to well-typed terms.

There's a few things we'll need to do in order to make this happen, starting with some new generators.

<!--[Read on!](nb/testing.html)-->
Coming soon...

# Exercises for the adventurous
- Devise the semantics and write the implementation for an operation in *NB* that determines whether two natural numbers are equal.
- Devise the semantics and write the implementation for operations that bridge *I* and *B*.
  - Include at least `==`, `/=`, `<=`, `<`, `>=`, `>`.
      - Use the precedence and associativity as reported by GHCi, via `:i (==)` etc...
- Write the `QuickCheck` to test progress and preservation hold for *NB*.
