---
title: N - the language of natural numbers
published: 2016-06-10 12:00:00+10:00
---

# Introducing *N*

We've just finished covering the [B language](./b.html).

Now we're going to look at the *N* language, which deals with natural numbers.

This is going to be a relative short tour, since you've seen most of this before.
The important points are that we'll be dealing with inductively defined values, and we'll be looking briefly at the difference between strict and lazy evaluation.

# Terms of *N*

The natural numbers are defined as:

- $\text{O}$, for the number zero
- $\text{S n}$, for the successor of the number $\text{n}$

The terms of *N* are as follows:

$$
\begin{aligned}
t =& \ \text{O} \\
  |& \ \text{S t} \\
  |& \ \text{pred t}
\end{aligned}
$$

We are including $\text{pred n}$ here, which represents the predecessor of $\text{n}$.
Since we don't have negative numbers, we define $\text{pred O = O}$.

We can easily create a `Term` type in Haskell:
```haskell
data Term =
    TmZero
  | TmSucc Term
  | TmPred Term
```

# Semantics for *N*

## Values

We've already mentioned that the natural numbers are either zero or the successor of a natural number.
These will be the form of the values of *N*.

For the zero case, we have:

$$
\prftree[r]{V-Zero}
{\text{O val}}
$$

and:
```haskell
valueTmZero :: Term
            -> Maybe Term
valueTmZero TmZero =
  Just TmZero
valueTmZero _ =
  Nothing
```

For the successor case, we have:

$$
\prftree[r]{V-Succ}
{\text{x val}}
{\text{(S x) val}}
$$

and:
```haskell
valueTmSucc :: (Term -> Maybe Term)
            -> Term
            -> Maybe Term
valueTmSucc val (TmSucc tm) =
  TmSucc <$> val tm
valueTmSucc _ _ =
  Nothing
```

## Small-step rules

We have four small-step rules: two of them are reduction rules, and two of them are congruence rules.

The reduction rules define the predecessor function.

For `pred O` we have:

$$
\prftree[r]{E-PredZero}
{\text{pred O} \longrightarrow \text{O}}
$$

and:
```haskell
ePredZero :: Term
          -> Maybe Term
ePredZero (TmPred TmZero) =
  Just TmZero
ePredZero _ =
  Nothing
```

For `pred (S n)` we have:

$$
\prftree[r]{E-PredSucc}
{\text{pred (S $v_1$)} \longrightarrow \text{$v_1$}}
$$

and:
```haskell
ePredSucc :: Term
          -> Maybe Term
ePredSucc (TmPred (TmSucc tm)) =
  value tm
ePredSucc _ =
  Nothing
```

The two congruence rules deal are used to keep things moving on when `S` and `pred` are wrapped around something other than a value.

The rule for `S` is:

$$
\prftree[r]{E-Succ}
{t_1 \longrightarrow t_2}
{\text{S $t_1$} \longrightarrow \text{S $t_2$}}
$$

which corresponds to:
```haskell
eSucc :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eSucc step (TmSucc tm) = do
  tm' <- step tm
  return $ TmSucc tm'
eSucc _ _ =
  Nothing
```

and the rule for `pred` is:

$$
\prftree[r]{E-Pred}
{t_1 \longrightarrow t_2}
{\text{pred $t_1$} \longrightarrow \text{pred $t_2$}}
$$


which corresponds to:
```haskell
ePred :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
ePred step (TmPred tm) = do
  tm' <- step tm
  return $ TmPred tm'
ePred _ _ =
  Nothing
```

## Strict versus Lazy

The rules we have seen so far are for strict evaluation.

The usual distinction between strict and non-strict is related to whether evaluation continues inside of function abstraction or not.

We haven't covered functions yet, but we now have something similar.
Recall that constructors in Haskell are functions, and that natural numbers would be represented by:
```haskell
data Nat =
    O 
  | S Nat
```
so we have a function
```haskell
S :: Nat -> Nat
```

With strict evaluation we evaluate the arguments to `S` - using `V-Succ` and `E-Succ` - when we come across `S n` as the top-level term.

With lazy evaluation we don't evaluate the argument to `S` in either of those cases.
The rule `E-PredSucc` can still move things forward when we have a `S` inside of one of our terms, but the outer 'S's are left alone.

This means that for lazy evaluation we completely delete the `E-Succ` rule above and rely on the other rules to peek inside of the `S` constructor.

We also need to update the `V-Succ` rule so that it doesn't look inside of the `S` constructor, and so we have:

$$
\prftree[r]{V-Succ}
{\text{(S x) val}}
$$

and:
```haskell
valueTmSucc :: Term
            -> Maybe Term
valueTmSucc (TmSucc tm) =
  Just (TmSucc tm)
valueTmSucc _ =
  Nothing
```

For now we'll stick to strict evaluation, but we'll come back to lazy evaluation later in the series.

# The rest of the story

From this point, we just need to write the code for `QuickCheck` support, parsing and printing for *N* and we're done.

And that's all we need to do.  If we copy the tests and the REPL across from the *B* project into the *N* project, they'll still work.

That's partly due to using the same interface, but also partly due to the form and robustness of the PLT theorems that we're using for our properties.

We are going to look at more uni-typed language before we add some new concepts into the mix.
[Read on!](i.html) 

# Exercises for the adventurous
- Complete the `QuickCheck`, parsing and printing for *N*.
- Devise semantics for *I*, with terms for integer literals, `+`, `-`, `*` and `^`.
- Implement *I* in Haskell, including the semantics, `QuickCheck`, parsing and pretty-printing.
