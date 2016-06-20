---
title: B - the language of Booleans
published: 2016-06-10 12:00:00+10:00
---

# Introducing *B*

Throughout this series, I'll be introducing the mathematical notational and the Haskell code side-by-side.

For an easy win, let's start with the type system.

We'll refer to the types using variants on $T$, like $T_1$, $T_2$, etc...

The first thing we do is define what types we'll have in our language:

$$
\begin{aligned}
T =& \ \text{Bool} \\
\end{aligned}
$$

Flushed with success, we'll also implement it in Haskell:
```haskell
data Type =
  TyBool
```

It should be clear that we're starting with a fairly simple language.

Next, we need to define the terms of our language.

We'll refer to the terms using variants on $t$, like $t_1$, $t_2$, etc..., and for this language they are recursively defined as:

$$
\begin{aligned}
t =& \ \text{true} \\
  |& \ \text{false}  \\
  |& \ \text{if $t$ then $t$ else $t$}
\end{aligned}
$$

This already gets a bit more interesting, since we have choices when it comes to the implementation of the Abstract Syntax Tree (AST) in Haskell.

We can define the AST as this:
```haskell
data Term =
    TmBool Bool 
  | TmIf Term Term Term
```
or as this:
```haskell
data Term =
    TmTrue
  | TmFalse 
  | TmIf Term Term Term
```

For the time being, we're going to go with the second definition.
I only bring it up now because it demonstrates one of the tradeoffs involved with DSL design.

# DSL terminology

DSLs can have either a shallow embedding or a deep embedding.
Beyond that, there is a vague notion that one embedding might be a deeper embedding than another.

With a shallow embedding we don't have an AST at all, and the language is defined by a set of functions.
The parsing library Parsec is a great example of this.

With a deep embedding we have an AST, so we can manipulate the terms of our language directly.
Our `Term` type is an example of a deeply embedded DSL.

The less we have to borrow from the host language, the deeper our embedding is.
The first version of `Term` above borrows Haskell's `Bool` as a primitve, whereas the second version defines it's own.

That's a choice that will keep coming up for us, in a number of different areas.

The benefit of sticking to the shallow end of the pool is that we can reuse a lot of things from the host language.
This includes things from inside the language - like variable substitution, typeclasses and parametric polymorphism - which can make the DSL fast and extensible.
This also includes things from around the language - like debuggers, profilers, and REPLs - which can make the DSL convenient to use.

At the deeper end of things we explicitly model more of the DSL, and so borrow less from the host language.
We lose some of the benefits of a more shallow embedding, but we get an increase in the ability to manipulate and reason about programs and program fragments in the DSL.

We're going to go deeper (heh) into those tradeoffs later in this series.
We'll also play with some shallowly embedded DSLs to help us out.

# Aside: Abstraction and reuse

Suppose we want to perform some logical operations with our Boolean values.

We could do so directly in Haskell:
```haskell
tmNot :: Term 
      -> Term
tmNot x = TmIf x TmFalse TmTrue

tmAnd :: Term 
      -> Term 
      -> Term
tmAnd x y = TmIf x y TmFalse

tmOr :: Term 
     -> Term 
     -> Term
tmOr x y = TmIf x TmTrue y
```

Here we've called out to the host language in order to build these pieces, so we're going to have to assemble them using the host language as well.

An alternative would be to extend the terms of our language:

$$
\begin{aligned}
t =& \ \ldots \\
  |& \ \neg t  \\
  |& \ t \wedge t \\
  |& \ t \vee t
\end{aligned}
$$

With the corresponding change to the Haskell:
```haskell
data Term =
    ... 
  | TmNot Term
  | TmAnd Term Term
  | TmOr Term Term
```

If those are pieces that we want to be able to manipulate and reason about, that is a good option.
We could use all kinds of identities to optimise terms in the language, like $not \ldotp not = id$.
If we do decide to change the terms of our language, we really want to minimize how invasive that change is to the rest of our DSL.

If those kinds of terms don't interest us, then we'd really like something in-between.
We'd like the ability to create and use these new concepts from within the DSL, without having to modify the terms of our language.
That means we should be able to create and use functions inside our DSL.

We'll be returning to both of these points soon, but they're going to be very important in this series.

# An evaluator

Let's build an evaluator for our language.
Evaluating a term in *B* should reduce to `TmFalse` or `TmTrue`.

Wait, should it?

Meh, let's just dive in:
```haskell
eval :: Term 
     -> Term
eval TmTrue = 
  TmTrue
eval TmFalse = 
  TmFalse
eval (TmIf t1 t2 t3) =
  case eval t1 of
    TmTrue  -> t2
    TmFalse -> t3
```

That looks right, doesn't it?
(Hint: it's not)

Anyhow, this is clearly nuts.
We need a way to talk about what it means for our evaluator to be correct.
At this point we're even a bit unclear about what the output of our evaluation function should be.

This is where semantics comes in.
There are a few different flavours of semantics, but we'll be focusing on small-step operational semantics, and they're the topic of the next post.
[Read on!](b/semantics.html)

# Exercises for the adventurous

- Write a correct version of `eval`.
- Write another correct version of `eval`, that proceeds in a different order to the first one.
- Write some tests for the two `eval` functions.
