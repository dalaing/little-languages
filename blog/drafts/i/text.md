---
title: Printing and Parsing *I*
published: 2016-05-27 12:00:00+10:00
---

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

As was the case with [*N*](n.html), we don't need to change the tests or the REPL, and they both continue to work.

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

# Clutching defeat from the jaws of victory 

There's a problem though - `needsParens` is incorrect.

A [post on reddit](https://www.reddit.com/r/haskell/comments/4pdfkx/symbolic_computer_algebra_print_tree/) covered some similar ground, but with different code for the equivalent of `needsParens`.

Clearly it's time for more `QuickCheck` in order to sort out what is going on.
[Read on!](testing.html)

# Exercises for the adventurous
- Add the logic operators for conjunction and disjunction to *B* , using `&&` and `||` as infix operators with the same precedence and associativity as reported by GHCi with `:i (&&)` etc...
    - Make sure that the semantics cause them to short-circuit appropriately.
- Add the logic operator for negation to *B* using `~` as a prefix operator.
