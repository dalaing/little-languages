---
title: A Type System for B
published: 2016-05-19 12:00:00+10:00
---

[Previously](testing.html) we wrote some tests for the semantics of the *B* language.

At the moment, we're pretty confident that *B* doesn't have any stuck terms.

Let us put that confidence to one side and write the typing rules for *B*.

This will introduce the ideas and the notation - but it is also a great introduction to the incremental nature of these pieces of PLT.
Eventually we're going to add more types to our language - and when we do that, we won't need to change the typing rules for Booleans or the properties about the type system.

Let's jump in.

# Type system rules

We use another set of rules for types, which indicates the relationship between terms and types:
$$
{\text{term} \colon \text{type}}
$$

As usual, the rules for the $\text{true}$ and $\text{false}$ cases are the easy ones:
$$
\prftree[r]{T-True}
{\text{true} \colon \text{Bool}}
$$

$$
\prftree[r]{T-False}
{\text{false} \colon \text{Bool}}
$$

An if-expression is well typed when test expression has type $\text{Bool}$ and when both branches have the same type.
When those conditions hold, the type of the if-expression is the same as the type of the branches.

Expressed in our rule system we have:
$$
\prftree[r]{T-If}
{t_1 \colon \text{Bool}}
{t_2 \colon \text{T}}
{t_3 \colon \text{T}}
{\text{if $t_1$ then $t_2$ else $t_3$} \colon \text{T}}
$$

TODO ill-typed term that isn't stuck, conservative type systems

# Type errors

TODO normal forms and type errors

```haskell
data TypeError =
    UnknownType
  | Unexpected { actual :: Type, expected :: Type }
  | ExpectedEq { type1 :: Type, type2 :: Type }
  deriving (Eq, Ord,Show)

expect :: MonadError TypeError m 
       => Type 
       -> Type 
       -> m ()
expect actual expected =
  unless (actual == expected) $
    throwError $ Unexpected actual expected

expectEq :: MonadError TypeError m 
       => Type 
       -> Type 
       -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwError $ ExpectedEq ty1 ty2
```

# Typing rules in Haskell

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
inferTmIf inferTerm (TmIf tm1 tm2 tm3) = Just $ do
  ty1 <- inferTerm tm1
  expect ty1 TyBool
  ty2 <- inferTerm tm2
  ty3 <- inferTerm tm3
  expectEq ty2 ty3
  return ty3
inferTmIf _ _ =
  Nothing
```

```haskell
inferTermRules :: MonadError TypeError m
               => [Term -> Maybe (m Type)]
inferTermRules =
  [ inferTmFalse
  , inferTmTrue
  , inferTmIf inferTerm
  ]

inferTerm :: MonadError TypeError m
          => Term
          -> m Type
inferTerm tm =
  fromMaybe (throwError UnknownType) .
  asum .
  fmap ($ tm) $
  inferTermRules
```

```haskell
runInfer :: Except e a -- ^
         -> Either e a -- ^
runInfer = runExcept
```

# Properties for type systems

# Testing the properties

# Exercises for the adventurous
