---
title: I - the language of integers
published: 2016-05-27 12:00:00+10:00
---

# Introducing *I*

We [previously](./n.md) looked at the language *N*.
Now we're going to have a look at *I*, a language built around integer expressions.

This will show how small-step semantics can be used to specify evaluation order, but will also give us some experience with more interesting parsers and pretty printers.

# Term of *I*

We will have terms for integer literals, as well as addition, subtraction, multiplication and exponentiation:

$$
\begin{aligned}
t =& \ \text{I int} \\
  |& \ \text{t} + \text{t} \\
  |& \ \text{t - t} \\
  |& \ \text{t} \times \text{t} \\
  |& \ \text{t}^\text{t}
\end{aligned}
$$

We'll represent these terms in Haskell with:
```haskell
data IntTerm tm =
    TmInt Int
  | TmAdd tm tm
  | TmSub tm tm
  | TmMul tm tm
  | TmExp tm tm
```

# Semantics for *I*

There is only one kind of values in *I*, the integer literals:

$$
\prftree[r]{V-Int}
{\text{(I i) val}}
$$

```haskell
valueTmInt :: Term
           -> Maybe Term
valueTmInt (TmInt i) =
  Just (TmInt i)
valueTmInt _ =
  Nothing
```

Most of the steps have one reduction rule which does the required arithmetic:

$$
\prftree[r]{E-AddIntInt}
{I (i_1) + I (i_2) \longrightarrow I (i_1 + i_2)}
$$

```haskell
eAddIntInt :: Term
           -> Maybe Term
eAddIntInt (TmAdd (TmInt i1) (TmInt i2)) =
  Just (TmInt (i1 + i2))
eAddIntInt _ =
  Nothing
```

and two congruence rules that control the evaluation order.

The first of the congruence rules causes the left-most argument to take a step:

$$
\prftree[r]{E-Add1}
{t_1 \longrightarrow t_1^{\prime}}
{t_1 + t_2 \longrightarrow t_1^{\prime} + t_2}
$$

```haskell
eAdd1 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eAdd1 step (TmAdd tm1 tm2) =
  TmAdd <$> step tm1 <*> pure tm2
eAdd1 _ _ =
  Nothing
```

and the second rule causes the right-most argument to take a step whenever the left-most argument is a value:

$$
\prftree[r]{E-Add2}
{t_2 \longrightarrow t_2^{\prime}}
{v_1 + t_2 \longrightarrow v_1 + t_2^{\prime}}
$$

We're using the variables names $\text{v}$ to represent values as a convenient shorthand, however we could have written the rule as:

$$
\prftree[r]{E-Add2}
{\text{value} \ t_1}
{t_2 \longrightarrow t_2^{\prime}}
{t_1 + t_2 \longrightarrow t_1 + t_2^{\prime}}
$$

which better matches the Haskell version we have:
```haskell
eAdd2 :: (Term -> Maybe Term)
      -> Term
      -> Maybe Term
eAdd2 step (TmAdd tm1 tm2) =
  TmAdd <$> value tm1 <*> step tm2
eAdd2 _ _ =
  Nothing
```

The semantics for subtraction: 

$$
\prftree[r]{E-SubIntInt}
{I (i_1) - I (i_2) \longrightarrow I (i_1 - i_2)}
$$

$$
\prftree[r]{E-Sub1}
{t_1 \longrightarrow t_1^{\prime}}
{t_1 - t_2 \longrightarrow t_1^{\prime} - t_2}
$$

$$
\prftree[r]{E-Sub2}
{t_2 \longrightarrow t_2^{\prime}}
{v_1 - t_2 \longrightarrow v_1 - t_2^{\prime}}
$$

and for multiplication:

$$
\prftree[r]{E-MulIntInt}
{I (i_1) \times I (i_2) \longrightarrow I (i_1 \times i_2)}
$$

$$
\prftree[r]{E-Mul1}
{t_1 \longrightarrow t_1^{\prime}}
{t_1 \times t_2 \longrightarrow t_1^{\prime} \times t_2}
$$

$$
\prftree[r]{E-Mul2}
{t_2 \longrightarrow t_2^{\prime}}
{v_1 \times t_2 \longrightarrow v_1 \times t_2^{\prime}}
$$

are defined similarly.

Exponentiation is a little different.
The congruence rules follow the familiar pattern, but we have two reduction rules.

We can't work with negative exponents of integers and continue to get integer results, so we define different rules for the cases of negative and non-negative exponents:

$$
\prftree[r]{E-ExpIntInt}
{0 <= i_2}
{{I (i_1)} ^ {I (i_2)} \longrightarrow I ({i_1}^{i_2})}
$$

$$
\prftree[r]{E-ExpIntNeg}
{i_2 < 0}
{{I (i_1)}^{I (i_2)} \longrightarrow I(0)}
$$

To be honest, I wrote the first version of *I* very late at night, and the semantics and associated code was written mostly by muscle memory.
This is just one of the things that `QuickCheck` caught while I was trading sleep for talk preparation.

# Parsing and printing

At this point we can turn the handle to write the code we need to test this with `QuickCheck`.

The parsing and printing code will be a bit more challenging.
Fortunately there are some great libraries for this, which is where we're going next.
[Read on!](i/text.html)

# Exercises for the adventurous
- Write a version of I with semantics that evaluate the right-arguments of the expressions first, and use `QuickCheck` to test that the behaviour is the same.
- Complete the `QuickCheck`, parsing and printing for *I*.


