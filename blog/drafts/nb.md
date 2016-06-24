---
title: NB - our first combination 
published: 2016-05-27 12:00:00+10:00
---

Previously we've looked at a couple of different little languages - [*B*](b.html), [*N*](n.html) and [*I*](i.html).

So far we've been able to get away with reusing the tests suites and REPL, and things have been going smoothly.

# Combining *N* and *B*

Let's see what happens if we put together a project with a term type that combines the terms from *N* and *B*:

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

## Type errors

While we ignored the ill-typed terms when specifying the typing rules, they're the first thing we need to deal with when writing code for our type system.

```haskell
data TypeError =
    Unexpected Type Type
  | ExpectedEq Type Type
  | NoMatchingTypeRule
  deriving (Eq, Ord, Show)
```

We're including `NoMatchingTypeRule` here so that we have a value we can return if no rule applies to a term.
We _could_ have left that out and used `Maybe TypeErrro` in its place but we didn't.
This is mostly to avoid a form of Boolean blindness - we don't want people (i.e. me, later on) - thinking that we are missing a `TypeError`, so we encode the reason or reasons that lead to us not having more information available.

Later on we'll test that type inference never results in `NoMatchingTypeRule`, after which we'll breathe a sigh of relief.

TODO explain error handling

```haskell
expect :: MonadError TypeError m
       => Type
       -> Type
       -> m ()
expect actual expected =
  unless (actual == expected) $
    throwError $ Unexpected actual expected
```

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

```haskell
inferTmZero :: Monad m
            => Term
            -> Maybe (m Type)
inferTmZero TmZero = Just $
  return TyNat
inferTmZero _ =
  Nothing
```

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

```haskell
inferTmFalse :: Monad m
             => Term
             -> Maybe (m Type)
inferTmFalse TmFalse = Just $
  return TyBool
inferTmFalse _ =
  Nothing
```

```haskell
inferTmTrue :: Monad m
            => Term
            -> Maybe (m Type)
inferTmTrue TmTrue = Just $
  return TyBool
inferTmTrue _ =
  Nothing
```

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

# Updating the REPL

Much like the 
The parsing and pretty printing rules 

TODO printing out the type information

# Properties of type systems

Type safety

Progress

Preservation

Why this makes sense

[Read on!](nb/testing.html)

# Exercises for the adventurous
- Devise the semantics and write the implementation for an operation in *NB* that determines whether two natural numbers are equal.
- Devise the semantics and write the implementation for operations that bridge *I* and *B*.
  - Include at least `==`, `/=`, `<=`, `<`, `>=`, `>`.
      - Use the precedence and associativity as reported by GHCi, via `:i (==)` etc...
- Write the `QuickCheck` to test progress and preservation hold for *NB*.
