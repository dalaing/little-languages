---
title: I - the language of integers
published: 2016-05-27 12:00:00+10:00
---

# Introducing *I*

We're going to have a look at *I*, a language built around integer expressions.

This is mostly to get some experience with more interesting parsers and pretty printers before we get bring in some more theory.

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

# Parsing *I*

We haven't had to parse any operators in our other languages, and I really don't want to go through the ins and outs of coming up with a grammar and factoring it appropriately (because there are other sources that do it better), so it's time to introduce some new tricks.

## Expression parsing with `parsers`

As usual, `parsers` has something useful for this.
In this case it's contained in `Text.Parser.Expression`, and we're going to do a bit of a lightning tour of what that offers.

This provides the data type `Assoc`:
```haskell
data Assoc =
    AssocNone
  | AssocLeft
  | AssocRight
```
the data type `Operator`:
```haskell
data Operator m a =
    Infix (m (a -> a -> a)) Assoc
  | Prefix (m (a -> a))
  | Postfix (m (a -> a))
```
the type synonym `OperatorTable`:
```haskell
type OperatorTable m a = [[Operator m a]]
```
and a function to convert an `OperatorTable` into a parser:
```haskell
buildExpressionParser :: forall m a. (Parsing m, Applicative m) 
                      => OperatorTable m a 
                      -> m a 
                      -> m a 
```

The second argument to `buildExpressionParser` is a parser for everything-but-the-operators.

If we were using this directly, we could make an `Operator` for each of our operators, like so:
```haskell
addOp :: (Monad m, TokenParsing m) 
      => Operator m Term
addOp = Infix parseAdd AssocLeft
  where
    parseAdd = TmAdd <$ symbol "+"
```

Once we had all of these defined, we'd put them into a list of lists, where the inner lists contained operators with the same precedence and the outer list is sorted in order of descending precedence:
```haskell
opTable :: (Monad m, TokenParsing m) 
        => OperatorTable m Term
opTable = [
    [expOp]
  , [mulOp]
  , [addOp, subOp]
  ]
```

Then all we'd need to do is to stitch everything together with `buildExpressionParser`:
```haskell
parseTerm = 
    buildExpressionParser table parseNotAnOperator <?> "term"
  where
    parseNotAnOperator = parseTmInt <|> parens parseTerm
```

The expression parser doesn't deal with parentheses, so we handle those in the other case.
This will handle things like "(2 + 3) * 5" along with "(((5)))".
The latter has no operators in it, so the expression parser keeps falling through to `parseNotAnOperator` until the parentheses are dealt with.

## Some helpers for expression parsers

We're going to shuffle some things around a little.
Most of the payoff will be in being able to continue to work with a list of parsing rules that we can combine, but it'll also keep the door open for adding user-defined operators with precedence annotations later on.

We start with a datatype capture associativity and precedence:
```haskell
data OperatorInfo =
  OperatorInfo {
    assoc      :: Assoc
  , precedence :: Int
  } deriving (Eq, Ord, Show)
```

We then create a datatype for our parsing rules:
```haskell
data ParseRule m t =
    ParseRegular (m t)
  | ParseOp OperatorInfo (m (t -> t -> t))
```

The `ParseRegular` rules are for the all of the parsers that we've seen so far.
The `ParseOp` rule combines the operator information with the parser for the operator, in the style needed by `Text.Parser.Expression`.

At some point we're going to need to partition these two types of rules.

For regular parsers, we just gather the parsers into a `Maybe`:
```haskell
gatherRegular :: ParseRule m t
              -> Maybe (m t)
gatherRegular (ParseRegular p) =
  Just p
gatherRegular _ =
  Nothing
```
so that we can use `mapMaybe` over the rules to get hold of the regular rules on their own.

For the operator parsers, we build a singleton `OperatorTable`, and pad it out using the precedence information:
```haskell
gatherOp :: ParseRule m t
         -> Maybe (OperatorTable m t)
gatherOp (ParseOp (OperatorInfo assoc prec) p) =
  Just $ [Infix p assoc] : replicate prec []
gatherOp _ =
  Nothing
```

If we are using `gatherOp` to partition parsing rules, we'll end up with a list of `OperatorTable`s, so we need a way to combine them:
```haskell
combineTables :: [OperatorTable m a]
              -> OperatorTable m a
combineTables os =
    foldr (zipWith (++) . pad) (pad []) os
  where
    -- find the operator with the highest precedence
    l = maximum . map length $ os
    -- pad everything out to that precedence
    pad ls = replicate (l - length ls) [] ++ ls
```

We should stop and look a bit more closely at the above functions.
The look like they're doing what they're meant to, but there are a few uses of `replicate`, `length` and `++` as well as the use of `maximum`.

The odds are good that we're doing more work than we need to.
I'm fine with that for a blog post that isn't about performance, as long as the meaning and correctness of the function is clear - however that isn't necessarily a zero-sum game.

Let's have another go.

For a particular rule, the information in the `OperatorTable` is made up of the precedence, the associativity and the parser.
The associativity and the parser get combined into an `Operator`, so we'll do that in `gatherOp`:
```haskell
gatherOp :: ParseRule m t
         -> Maybe (Int, Operator m t)
gatherOp (ParseOp (OperatorInfo assoc prec) p) =
  Just (prec, Infix p assoc)
gatherOp _ =
  Nothing
```

The `OperatorTable` is a list of list of `Operators`, where the `Operator`s in the inner lists all have the same precedence, and the outer list is sorted by descending order of the precedence.

Our function signature is:
```haskell
createTable :: [(Int, Operator m a)]
              -> OperatorTable m a
createTable os = 
  ...
```

We can sort the our list by descending precendence by using `sortOn` from `Data.List`.
We'll also use `Down` from `Data.Ord` to get a descending sort.

So far we have:
```haskell
import Data.List (sortOn)
import Data.Ord  (Down(..))

createTable :: [(Int, Operator m a)]
              -> OperatorTable m a
createTable os = 
    ... .
    sortOn (Down . fst) $
    os
```

Before `sortOn` was added we would have had to have used `sortBy (comparing (Down . fst))`.
The behaviour is the same, but `sortOn` is faster.

Now we need to group the entries with the same precedence.

We can use `groupBy` for this.
It groups adjacent elements using a custom equality test.
The adjacency is handled already because we just sorted the list, so we need to check for equal precedences.
We can use `on` from `Data.Function` to help with this.

We're almost there:
```haskell
import Data.List     (sortBy, groupBy)
import Data.Ord      (comparing, Down(..))
import Data.Function (on)


createTable :: [(Int, Operator m a)]
              -> OperatorTable m a
createTable os = 
    ... .
    -- we now have [[(Int, Operator m a)]]
    groupBy ((==) `on` fst) .
    sortBy (comparing (Down . fst)) $
    os
```

We can now get rid of the precedence information and eta-reduce the function:
```haskell
createTable :: [(Int, Operator m a)]
              -> OperatorTable m a
createTable =
    fmap (fmap snd) .
    groupBy ((==) `on` fst) .
    sortBy (comparing (Down . fst))
```
and we have a snazy little pipeline that doesn't look to be doing too much in the way of redundant work.

If you haven't had a browse through the `base` package in a while, it's worth doing every now and then - there's lots of neat stuff in there, and if it's been a while you might spot some new functions (or new uses for old and familiar functions).

Now we can use all of the above to build a parser from the rules.

We gather the regular parsers together into a parser for non-operators, we gather the operator parsers into an `OperatorTable`, and we use `buildExpressionParser` to combine them:
```haskell
mkParser :: TokenParsing m
         => [ParseRule m t]
         -> m t
mkParser rules =
  let
    parseRegular =
      (<|> parens parseTerm) .
      asum .
      mapMaybe gatherRegular $
      rules
    tables =
      createTable .
      mapMaybe gatherOp $
      rules
    parseTerm =
      buildExpressionParser tables parseRegular
  in
    parseTerm
```

## Putting this to use with *I*

We're still working with a token parser, so we'll define a style for operators:
```haskell
operatorStyle :: TokenParsing m
              => IdentifierStyle m
operatorStyle =
    IdentifierStyle {
      _styleName              = "operator"
    , _styleStart             = _styleLetter operatorStyle
    , _styleLetter            = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , _styleReserved          = HS.fromList reservedOperators
    , _styleHighlight         = Operator
    , _styleReservedHighlight = ReservedOperator
    }
  where
    reservedOperators =
      ["+", "-", "*", "^"]
```
and our usual helper function:
```haskell
reservedOperator :: (Monad m, TokenParsing m)
                 => String
                 -> m ()
reservedOperator =
  reserve operatorStyle
```

From there, we define a parse for integer literals:
```haskell
parseTmInt :: (Monad m, TokenParsing m)
           => m Term
parseTmInt =
  (TmInt . fromInteger) <$> integer <?> "Int"
```
and parsers for all of operators:
```haskell
parseTmAdd :: (Monad m, TokenParsing m)
           => m (Term -> Term -> Term)
parseTmAdd =
  TmAdd <$ reservedOperator "+" <?> "+"

parseTmSub :: (Monad m, TokenParsing m)
           => m (Term -> Term -> Term)
parseTmSub =
  TmSub <$ reservedOperator "-" <?> "-"

parseTmMul :: (Monad m, TokenParsing m)
           => m (Term -> Term -> Term)
parseTmMul =
  TmMul <$ reservedOperator "*" <?> "*"

parseTmExp :: (Monad m, TokenParsing m)
           => m (Term -> Term -> Term)
parseTmExp =
  TmExp <$ reservedOperator "^" <?> "^"
```

We package these up into our new `ParseRule` data structure:
```haskell
parseTermRules :: (Monad m, TokenParsing m)
               => [ParseRule m Term]
parseTermRules =
  [ ParseRegular parseTmInt
  , ParseOp (OperatorInfo AssocLeft 6) parseTmAdd
  , ParseOp (OperatorInfo AssocLeft 6) parseTmSub
  , ParseOp (OperatorInfo AssocLeft 7) parseTmMul
  , ParseOp (OperatorInfo AssocRight 8) parseTmExp
  ]
```
with precedence and associativity information stolen from Haskell, and then we build our parser:
```haskell
parseTerm :: (Monad m, TokenParsing m)
          => m Term
parseTerm =
  mkParser parseTermRules <?> "term"
```

We than have the option of smothering this in doctests.

I'd be a little more cautious with that recommendation if I thought there was going to be a high rate of churn for these examples. 
We won't be expecting anything that parses now to stop parsing when we add orthogonal features to our language, so we won't have to update these examples unless we've broken something.

With that in mind, lets go nuts:
```haskell
-- | The parser for terms of the I language.
--
-- This function is built from the contents of 'parseTermRules',
-- with added support for parentheses.
--
-- We can parse all of the simple forms of the terms:
-- >>> parse parseTerm "3"
-- Success (TmInt 3)
--
-- >>> parse parseTerm "2 + 5"
-- Success (TmAdd (TmInt 2) (TmInt 5))
--
-- >>> parse parseTerm "2 - 5"
-- Success (TmSub (TmInt 2) (TmInt 5))
--
-- >>> parse parseTerm "2 * 5"
-- Success (TmMul (TmInt 2) (TmInt 5))
--
-- >>> parse parseTerm "2 ^ 5"
-- Success (TmExp (TmInt 2) (TmInt 5))
--
-- The left associative operators group to the left:
-- >>> parse parseTerm "3 - 2 + 5"
-- Success (TmAdd (TmSub (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- So brackets grouping things on the left are redundant:
-- >>> parse parseTerm "(3 - 2) + 5"
-- Success (TmAdd (TmSub (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- We need brackets if we want to group things on the right:
-- >>> parse parseTerm "3 - (2 + 5)"
-- Success (TmSub (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
--
-- The right associative operator groups to the right:
-- >>> parse parseTerm "3 ^ 2 ^ 5"
-- Success (TmExp (TmInt 3) (TmExp (TmInt 2) (TmInt 5)))
--
-- So brackets grouping things on the right are redundant:
-- >>> parse parseTerm "3 ^ (2 ^ 5)"
-- Success (TmExp (TmInt 3) (TmExp (TmInt 2) (TmInt 5)))
--
-- We need brackets if we want to group things on the left:
-- >>> parse parseTerm "(3 ^ 2) ^ 5"
-- Success (TmExp (TmExp (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- Multiplication binds more tightly than addition:
-- >>> parse parseTerm "3 * 2 + 5"
-- Success (TmAdd (TmMul (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- >>> parse parseTerm "3 + 2 * 5"
-- Success (TmAdd (TmInt 3) (TmMul (TmInt 2) (TmInt 5)))
--
-- So we need to use brackets multiply by the sum of two terms:
-- >>> parse parseTerm "3 * (2 + 5)"
-- Success (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
--
-- >>> parse parseTerm "(3 + 2) * 5"
-- Success (TmMul (TmAdd (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- Exponentiation binds more tightly than multiplication:
-- >>> parse parseTerm "3 ^ 2 * 5"
-- Success (TmMul (TmExp (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- >>> parse parseTerm "3 * 2 ^ 5"
-- Success (TmMul (TmInt 3) (TmExp (TmInt 2) (TmInt 5)))
--
-- So we need to use brackets if the exponent or the power is a product (or sum) of two terms:
-- >>> parse parseTerm "3 ^ (2 * 5)"
-- Success (TmExp (TmInt 3) (TmMul (TmInt 2) (TmInt 5)))
--
-- >>> parse parseTerm "(3 * 2) ^ 5"
-- Success (TmExp (TmMul (TmInt 3) (TmInt 2)) (TmInt 5))
--
-- Nonsense is still rejected:
-- >>> parse parseTerm "potato"
-- Failure (interactive):1:1: error: expected: term
-- potato<EOF>
-- ^
--
-- Even if you try to hide it in brackets:
-- >>> parse parseTerm "((potato))"
-- Failure (interactive):1:3: error: expected: operator
-- ((potato))<EOF>
--   ^
parseTerm :: (Monad m, TokenParsing m)
          => m Term
parseTerm =
  mkParser parseTermRules <?> "term"
```

# Pretty Printing *I*

When it comes to pretty printing, we'd really like to be able to print out our terms 
- so that we can parse them back in again and get the original term
- with the minimal number of brackets required

In fact, we should be able to flip our doc tests from the parse around and get the same results, except not including the results with redundant brackets or potatoes.

That's what we'll work towards.

## The obligatory helper functions


We're going to create a data type for pretty printing rules, and it's going to be similar to `ParseRule` from earlier:
```haskell
data PrettyRule a =
    PrettyRegular (a -> Maybe Doc)
  | PrettyOp OperatorInfo (a -> Maybe (a, a)) (Doc -> Doc -> Doc)
```

The `PrettyRegular` variant is for everything other than operators, and it pretty prints `a` if the rule matches.
The `PrettyOp` variant is for the operators, and includes the precedence and associativity information for the operator, a matching function tat returns the arguments to the operator, and a pretty printing function for the operator that requires that both arguments are pretty printed before it is used.

We'll be dealing with the regular and operator pretty printers separately, so again we'll need functions to partition the two types of rules.

For the regular pretty printing rules, we gather the pretty printers into a `Maybe`:
```haskell
gatherRegular :: a
              -> PrettyRule a
              -> Maybe Doc
gatherRegular t (PrettyRegular f) =
  f t
gatherRegular _ _ =
  Nothing
```
so that we can use `mapMaybe` over the rules to get hold of the regular rules on their own.

The pretty printing rules for operators require a couple of helper functions.

We need a function that will search through the rules to find the `OperatorInfo` for a term, if it exists:
```haskell
findOperatorInfo :: [PrettyRule a]
                 -> a
                 -> Maybe OperatorInfo
findOperatorInfo rules tm =
    asum .
    fmap (checkOperatorInfo tm) $
    rules
  where
    checkOperatorInfo t (PrettyOp info match _) =
      info <$ match t
    checkOperatorInfo _ _ =
      Nothing
```

We also need a function to determine if the argument to an operator needs parentheses.

First we add a data type to describe the arguments to an operator:
```haskell
data Argument =
    ArgumentLeft
  | ArgumentRight
  deriving (Eq, Ord, Show)
```
and a function that works out if an argument is in the right position for it to associate:
```haskell
argumentAssociates :: Argument
                   -> Assoc
                   -> Bool
argumentAssociates ArgumentLeft AssocLeft =
  True
argumentAssociates ArgumentRight AssocRight =
  True
argumentAssociates _ _ =
  False
```

We could have used `Assoc` here and not written `Argument`, but new data types and functions are cheap and I didn't want to leave the door open to passing in `AssocNone` by accident in the future and getting unexpected results.

Now we're ready for the `needsParens` function:
```haskell
needsParens :: Argument
            -> OperatorInfo
            -> Maybe OperatorInfo
            -> Bool
```
which takes the position of the argument to an operator, the operator information for the current operator and for the argument to the operator - the result of `findOperatorInfo` - and returns a Boolean indicating whether we should add parentheses or not.

If the argument wasn't an operator, then we don't need parentheses:
```haskell
needsParens _ _ Nothing =
  False
```

Otherwise, based on some playing around with doctests, we need parentheses if:
```haskell
needsParens arg info (Just argInfo) =
```
the operator has no associativity,
```haskell
  assoc info == AssocNone ||
```
the argument has a lower precedence than the current operator,
```haskell
  precedence argInfo < precedence info ||
```
or if the argument has the same precedence as the current operator but the argument
is not in the right position for it to associate
```haskell
  (precedence argInfo == precedence info && not (argumentAssociates arg (assoc info)))
```

Now we can prepare a pretty printer for operators.

Thanks to the laziness of Haskell, we can pass in the pretty printer for terms that we're trying to build.
We also pass in the result of `findOperatorInfo`.
```haskell
gatherOp :: a
         -> (a -> Doc)
         -> (a -> Maybe OperatorInfo)
         -> PrettyRule a
         -> Maybe Doc
```
If we're given an operator, we check to see if it matches the rule we're currently processing:
```haskell
gatherOp t pretty findInfo (PrettyOp i match printer) = do
  (t1, t2) <- match t
```
We then build little functions that will add parentheses if they are needed:
```haskell
  let addParens b =
    if b then parens else id
  let p1 =
    addParens .
    needsParens ArgumentLeft i .
    findInfo $
    t1
  let p2 =
    addParens .
    needsParens ArgumentRight i .
    findInfo $
    t2
```
and use them in conjunction with our overall pretty printer and the function that does the printing for the current operator:
```haskell
  return $ printer (p1 . pretty $ t1) (p2 . pretty $ t2)
```
In the case whre we're not given an operator, this function isn't for us:
```haskell
gatherOp _ _ _ _ =
  Nothing
```

We can use the above functions to build a pretty printing function from the pretty printing rules:
```haskell
mkPretty :: [PrettyRule a]
         -> a
         -> Doc
mkPretty rules =
  let
    prettyRegular tm =
      asum .
      fmap (gatherRegular tm) $
      rules
    findInfo =
      findOperatorInfo rules
    prettyOp tm =
      asum .
      fmap (gatherOp tm prettyTerm findInfo) $
      rules
    prettyTerm tm =
      fromMaybe (text "???") .
      asum . 
      fmap ($ tm) $
      [ prettyRegular , prettyOp ]
  in
    prettyTerm
```

We're building the pretty printer out of the pretty printers for regular terms and the pretty printer for operators.
The pretty printer for regular terms shouldn't be too surprising at this point.

The only trick with the pretty printer for the operators is that we assemble some functions and pass them in to `gatherOp`, so that we don't need to pass in all the rules.

That should be all we need, so it's time to press on.

## Putting this to use with *I*

The heavy lifting has already been done, so we just need to put together the pieces for *I*.

We have one fairly usually looking pretty printer for `TmInt`, making use of the `int` helper from `ansi-wl-pprint`:
```haskell
import           Text.Parser.Token.Highlight  (Highlight (..))
import           Text.Trifecta.Highlight      (withHighlight)

prettyTmInt :: Term
             -> Maybe Doc
prettyTmInt (TmInt i) =
  Just $ withHighlight Number (int i)
prettyTmInt _ =
  Nothing
```
We're stealing the highlighting from `trifecta` again, since the parser we use for `TmInt` adds the `Number` highlight under the hood.

We also have a pair of functions for each of our operators.
They're all pretty similar in this instance, so we'll only look at the code associated with `TmAdd`.

One of the functions checks that we're dealing with the operator we're interested in, and returns the arguments to the operator if that's the case:
```haskell
-- |
matchTmAdd :: Term
           -> Maybe (Term, Term)
matchTmAdd (TmAdd tm1 tm2) =
  Just (tm1, tm2)
matchTmAdd _ =
  Nothing

The other functions pretty prints the operator, given that the arguments to the operator have already been pretty pritned:
-- |
prettyTmAdd :: Doc
            -> Doc
            -> Doc
prettyTmAdd d1 d2 =
  d1 <+> reservedOperator "+" <+> d2
```

Once we have these functions, we can build a list of the pretty printing rules with the same operator information that we used for the parser:
```haskell
prettyTermRules :: [PrettyRule Term]
prettyTermRules =
  [ PrettyRegular prettyTmInt
  , PrettyOp (OperatorInfo AssocLeft 6) matchTmAdd prettyTmAdd
  , PrettyOp (OperatorInfo AssocLeft 6) matchTmSub prettyTmSub
  , PrettyOp (OperatorInfo AssocLeft 7) matchTmMul prettyTmMul
  , PrettyOp (OperatorInfo AssocRight 8) matchTmExp prettyTmExp
  ]
```

Then we combine them all into a pretty printing function:
```haskell
prettyTerm :: Term
           -> Doc
prettyTerm =
  mkPretty prettyTermRules
```

As usual, we then spray doctests all over the place:
```haskell
-- | The pretty printer for terms of the I language.
--
-- This function is built from the contents of 'prettyTermRules'.
-- It will print "???" if none of the rules apply - which should never happen.
--
-- We can print all of the simple forms of the terms:
-- >>> render 0.5 40 prettyTerm (TmInt 3)
-- 3
--
-- >>> render 0.5 40 prettyTerm (TmAdd (TmInt 2) (TmInt 5))
-- 2 + 5
--
-- >>> render 0.5 40 prettyTerm (TmSub (TmInt 2) (TmInt 5))
-- 2 - 5
--
-- >>> render 0.5 40 prettyTerm (TmMul (TmInt 2) (TmInt 5))
-- 2 * 5
--
-- >>> render 0.5 40 prettyTerm (TmExp (TmInt 2) (TmInt 5))
-- 2 ^ 5
--
-- The left associative operators don't need extra brackets when things are grouped to the left:
-- >>> render 0.5 40 prettyTerm (TmAdd (TmAdd (TmInt 3) (TmInt 2)) (TmInt 5))
-- 3 + 2 + 5
--
-- But they do need brackets when the grouping branches to the right:
-- >>> render 0.5 40 prettyTerm (TmAdd (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- 3 + (2 + 5)
--
-- The right associative operators don't need extra brackets when things are grouped to the right:
-- >>> render 0.5 40 prettyTerm (TmExp (TmInt 3) (TmExp (TmInt 2) (TmInt 5)))
-- 3 ^ 2 ^ 5
--
-- But they do need brackets when the grouping branches to the left:
-- >>> render 0.5 40 prettyTerm (TmExp (TmExp (TmInt 3) (TmInt 2)) (TmInt 5))
-- (3 ^ 2) ^ 5
--
-- Multiplication binds more tightly than addition, so we don't want any brackets if we multiply and then add:
-- >>> render 0.5 40 prettyTerm (TmAdd (TmMul (TmInt 3) (TmInt 2)) (TmInt 5))
-- 3 * 2 + 5
--
-- If we are adding and then multiplying, we'll want brackets:
-- >>> render 0.5 40 prettyTerm (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- 3 * (2 + 5)
--
-- This is true regardless of whether the addition is on the left or the right:
-- >>> render 0.5 40 prettyTerm (TmAdd (TmInt 3) (TmMul (TmInt 2) (TmInt 5)))
-- 3 + 2 * 5
--
-- >>> render 0.5 40 prettyTerm (TmMul (TmAdd (TmInt 3) (TmInt 2)) (TmInt 5))
-- (3 + 2) * 5
--
-- Exponentiation binds more tightly than multiplication, so we don't want any brackets if exponentiate and then multiply:
-- >>> render 0.5 40 prettyTerm (TmMul (TmExp (TmInt 3) (TmInt 2)) (TmInt 5))
-- 3 ^ 2 * 5
--
-- If we're multiplying and then exponentiating, we'll want brackets:
-- >>> render 0.5 40 prettyTerm (TmExp (TmInt 3) (TmMul (TmInt 2) (TmInt 5)))
-- 3 ^ (2 * 5)
prettyTerm :: Term
           -> Doc
prettyTerm =
  mkPretty prettyTermRules
```

Again, like with [*N*](n.html), we don't need to change the tests or the REPL, and they both continue to work.

It's always worth having a play around in the REPL at this point, as something somewhere between a double-check and a victory lap:
```haskell
> 2 + 3 * 5
2 + 3 * 5 ==> 17
> (2 + 3) * 5
(2 + 3) * 5 ==> 25
> 1 + 2 + 3
1 + 2 + 3 ==> 6
> 1 + (2 + 3)
1 + (2 + 3) ==> 6
```

# Cranking up the testing

There's a problem though - `needsParens` is incorrect.

A [post on reddit](https://www.reddit.com/r/haskell/comments/4pdfkx/symbolic_computer_algebra_print_tree/) covered some similar ground, but with different code for the equivalent of `needsParens`.

Clearly it's time for more `QuickCheck` in order to sort out what is going on.

We're going to build generators for a set of operators, and use that to generate arbitrary expressions.
Once we have that, we'll check that pretty printing the expression and parsing it back in returns the original expression.

We'll also check that we either a parsing failure or a different expression if we delete any matched pair of parentheses from the pretty printed string in the middle of that round trip test.

That seems like it should make sure that we're putting parentheses in the right places, and that we're not adding any redundant parentheses.

To start with, we'll need an `Arbitrary` instance for `OperatorInfo`:
```haskell
instance Arbitrary OperatorInfo where
  arbitrary =
    let
      genAssoc =
        elements [AssocNone, AssocLeft, AssocRight]
      genPrecedence =
        choose (0, 9)
    in
      OperatorInfo <$> genAssoc <*> genPrecedence
  shrink (OperatorInfo a p) =
    fmap (OperatorInfo a) (shrink p)
```

We're going to create some code to generate a table of operators.

We'll use a map from `String`s to `OperatorInfo`, wrapped in a `newtype` so that we can write an `Arbitrary` instance: 
```haskell
import qualified Data.Map as M

newtype OperatorMap = OperatorMap {
    getOperatorMap :: M.Map String OperatorInfo
  } deriving (Eq, Show)
```

The entries in the map are created from one of a number of single character names and a arbitrary `OperatorInfo`:
```haskell
genOperatorEntry :: Gen (String, OperatorInfo)
genOperatorEntry =
  (,) <$> elements (map pure "+=*&^%$#@!") <*> arbitrary
```

The `OperatorMap` is built from a non-empty list of these operators: 
```haskell
genOperatorMap :: Gen OperatorMap
genOperatorMap =
  fmap (OperatorMap . M.fromList) (listOf1 genOperatorEntry)
```

For the shrinking, we don't want to shrink the operator names in the entries:
```haskell
shrinkOperatorEntry :: (String, OperatorInfo)
                    -> [(String, OperatorInfo)]
shrinkOperatorEntry (s, oi) =
  fmap (\oi' -> (s, oi')) (shrink oi)
```
and we can use the `shrinkList` helper to build the shrinking function for the whole map:
```haskell
shrinkOperatorMap :: OperatorMap
                  -> [OperatorMap]
shrinkOperatorMap (OperatorMap m) =
  fmap (OperatorMap . M.fromList) .
  filter (not . null) .
  shrinkList shrinkOperatorEntry .
  M.toList $
  m
```

Now we have what we need for an `Arbitrary` instance:
```haskell
instance Arbitrary OperatorMap where
  arbitrary =
    genOperatorMap
  shrink =
    shrinkOperatorMap
```

Thus armed, we create a generic expression type.
We use the lowest-effort form of generics here: `String`.

```haskell
data Expr =
    Const Int
  | Op String Expr Expr
  deriving (Eq, Show)
```

We are going to need support for generation, pretty printing and parsing of `Expr`.

We can use the `OperatorMap` to generate these expressions:
```haskell
genExpr :: OperatorMap
        -> Gen Expr
genExpr =
  sized .
  genExpr'

genExpr' :: OperatorMap
         -> Int
         -> Gen Expr
genExpr' _ 0 =
    Const <$> arbitrary
genExpr' om@(OperatorMap m) s =
    oneof [
      Const <$> arbitrary
    , Op <$> elements (M.keys m) <*> child2 <*> child2
    ]
  where
    s2 = s `div` 2
    child2 = genExpr' om s2
```
and the shrinking function is straight-forward:
```haskell
shrinkExpr :: Expr
           -> [Expr]
shrinkExpr (Const _) =
  []
shrinkExpr (Op s e1 e2) =
  e1 :
  e2 :
  fmap (\e1' -> Op s e1' e2) (shrinkExpr e1) ++
  fmap (\e2' -> Op s e1 e2') (shrinkExpr e2)
```

The `OperatorMap` can be turned into a pretty printer fairly mechanically:
```haskell
toPrettyRules :: OperatorMap
              -> [PrettyRule Expr]
toPrettyRules (OperatorMap m) =
    intRule : map convertEntry (M.toList m)
  where
    intRule =
      PrettyRegular prettyInt
    prettyInt (Const i) =
      Just $ int i
    prettyInt _ =
      Nothing
    convertEntry (s, oi) =
      PrettyOp oi (matchOp s) (prettyOp s)
    matchOp s (Op t e1 e2)
      | s == t = Just (e1, e2)
      | otherwise = Nothing
    matchOp _ _ =
      Nothing
    prettyOp s d1 d2 =
      d1 <+> text s <+> d2

mkExprPretty :: OperatorMap
             -> Expr
             -> Doc
mkExprPretty m =
  mkPretty (toPrettyRules m)
```
and the same is true with the parser:
```haskell
toParserRules :: TokenParsing m
              => OperatorMap
              -> [ParseRule m Expr]
toParserRules (OperatorMap m) =
    intRule : map convertEntry (M.toList m)
  where
    intRule =
      ParseRegular parseInt
    parseInt =
      (Const . fromInteger) <$> integer
    convertEntry (s, oi) =
      ParseOp oi ((\s' -> Op s') <$> symbol s)

mkExprParser :: (TokenParsing m)
             => OperatorMap
             -> m Expr
mkExprParser m =
    mkParser (toParserRules m)
  where
```

At this point, we have the tools to write the property that checks that we can make a round trip through the pretty printer and then the parser:
```haskell
propPrettyParse :: OperatorMap
                -> Property
propPrettyParse om =
  let
    parseExpr = mkExprParser om
    prettyExpr = mkExprPretty om
    roundTrip =
      parseFromString parseExpr .
      prettyToString .
      prettyExpr
  in
    forAllShrink (genExpr om) shrinkExpr $ \expr ->
      case roundTrip expr of
        Left _ -> property False
        Right expr' -> expr === expr'
```

We'll do some quick and dirty utility functions to find the positions of match pairs of parentheses in a string:
```haskell
findParens :: String
           -> [(Int, Int)]
findParens =
    (\(x,_,_) -> x) . foldr f ([], [], 0 :: Int) . reverse
  where
    f '(' (done, opens, ix) =
      (done, ix : opens, ix + 1)
    f ')' (done, o : opens, ix) =
      ((o, ix) : done, opens, ix + 1)
    f _ (done, opens, ix) =
      (done, opens, ix + 1)
```
and to remove those parentheses from a string:
```haskell
removeParens :: String
             -> (Int, Int)
             -> String
removeParens str (s, e) =
  map fst .
  filter ((\i -> i /= s && i /= e) . snd) .
  zip str $
  [0..]
```

We can combine these to make something which produces a list of strings where each of the matched pairs of parentheses have been removed:
```haskell
parenRemovals :: String
                -> [String]
parenRemovals s =
  map (removeParens s) .
  findParens $
  s
```
This will return an empty list if there are no parentheses, and that's fine for what we're using it for.

Now we can test that we're using the minimal number of parentheses.
If that is the case, then removing a pair of parentheses will either cause the parsing to fail or will cause a change in what gets parsed back in:
```haskell
propPrettyParseParens :: OperatorMap
                      -> Property
propPrettyParseParens om =
  let
    parseExpr = 
      mkExprParser om
    prettyExpr = 
      mkExprPretty om
    roundTripsWithParensRemoved =
      fmap (parseFromString parseExpr) .
      parenRemovals .
      prettyToString .
      prettyExpr
  in
    forAllShrink (genExpr om) shrinkExpr $ \expr ->
      -- check all of the round-trips with a single pair
      -- of parens removed
      flip all (roundTripsWithParensRemoved expr) $ \rt ->
        case rt of
          Left _ ->
            True
          Right expr' ->
            expr /= expr'
```

Let's see how we go with these properties:
```
common
  text
    pretty-parse round trip:         FAIL (0.03s)
      *** Failed! Falsifiable (after 15 tests and 4 shrinks):
      OperatorMap {getOperatorMap = fromList [("#",OperatorInfo {assoc = AssocLeft, precedence = 0}),("%",OperatorInfo {assoc = AssocLeft, precedence = 0}),("&",OperatorInfo {assoc = AssocNone, precedence = 0})]}
      Op "#" (Op "&" (Const (-4)) (Const 4)) (Const 10)
      Op "#" (Op "&" (Const (-4)) (Const 4)) (Const 10) /= Op "&" (Const (-4)) (Const 4)
    pretty-parse correct parens:   FAIL
      *** Failed! Falsifiable (after 20 tests and 9 shrinks):
      OperatorMap {getOperatorMap = fromList [("$",OperatorInfo {assoc = AssocNone, precedence = 0}),("=",OperatorInfo {assoc = AssocNone, precedence = 1})]}
      Op "$" (Op "=" (Const 3) (Const (-13))) (Const 1)
```
Hmm, not well.

We can try shifting our code across to the code in the Reddit post, but since it didn't mention the `AssocNone` case we'll leave our handling of that alone:
```haskell
needsParens :: Argument
            -> OperatorInfo
            -> Maybe OperatorInfo
            -> Bool
needsParens _ _ Nothing =
  False
needsParens arg info (Just argInfo) =
  assoc argInfo == AssocNone || 
  ( precedence argInfo <= precedence info &&
    ( precedence argInfo /= precedence info || 
      assoc info /= assoc argInfo || 
      not (argumentAssociates arg (assoc info))
    )
  )
```

This improves things quite a bit:
```
common
  text
    pretty-parse round trip:         OK (0.05s)
      +++ OK, passed 100 tests.
    pretty-parse correct parens:   FAIL (0.01s)
      *** Failed! Falsifiable (after 9 tests and 8 shrinks):
      OperatorMap {getOperatorMap = fromList [("$",OperatorInfo {assoc = AssocNone, precedence = 0}),("%",OperatorInfo {assoc = AssocNone, precedence = 1})]}
      Op "$" (Const (-8)) (Op "%" (Const 8) (Const 0))
```
except we're putting too many parentheses in for the `AssocNone` case when other operators of the same precedence are in play.

It turns out that the code from the Reddit post handles the `AssocNone` case as it stands:
```haskell
needsParens :: Argument
            -> OperatorInfo
            -> Maybe OperatorInfo
            -> Bool
needsParens _ _ Nothing =
  False
needsParens arg info (Just argInfo) =
  precedence argInfo <= precedence info &&
  ( precedence argInfo /= precedence info || 
    assoc info /= assoc argInfo || 
    not (argumentAssociates arg (assoc info))
  )
```

Now the tests pass, and there is much rejoicing:
```
common
  text
    pretty-parse round trip:         OK
      +++ OK, passed 100 tests.
    pretty-parse correct parens:   OK
      +++ OK, passed 100 tests.
```

There was also a link in that Reddit thread to [a paper on unparsing](http://www.cs.tufts.edu/~nr/pubs/unparse-abstract.html) that is also worth a read.
On one hand, it would have been nice to have had the right search terms to find that earlier.
On the other hand, we now have some nice tests around our unparsing utilities.

Now it's definitely time to look at combining various languages.
[Read on!](nb.html)

# Exercises for the adventurous
- Add the logic operators for conjunction and disjunction to *B* , using `&&` and `||` as infix operators.
    - Make sure that the semantics cause them to short-circuit appropriately.
- Add the logic operator for negation to *B* using `~` as a prefix operator.

