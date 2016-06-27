---
title: Testing *I*
published: 2016-06-27 12:00:00+10:00
---

# Cranking up the testing

We're going to build generators for a set of operators, and use that to generate arbitrary expressions.
Once we have that, we'll check that pretty printing the expression and parsing it back in returns the original expression.

We'll also check that we either a parsing failure or a different expression if we delete any matched pair of parentheses from the pretty printed string in the middle of that round trip test.

That seems like it should make sure that we're putting parentheses in the right places, and that we're not adding any redundant parentheses.

# Arbitrary operators

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

# Arbitrary expressions

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

## Parsing and printing expressions

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

# The properties we need

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

So far, so good.

In order to test that remove parentheses in the middle of a round-trip alters the results, we'll need some quick and dirty utility functions.

The first one will find the positions of match pairs of parentheses in a string:
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
and the second one will remove those parentheses from a string:
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

# Trying them out

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
On the other hand, we now have some nice tests around our unparsing utilities, and I'm much more confident in this part of the code.

# Leveling up

We've now looked at a number of uni-typed languages.
The obvious next step is to combine a few of them and see what happens.

This is pretty exciting - we'll get to look at type systems, and the links between semantics and types, and there'll be a few other little tips and tricks in between.
<!--[Read on!](../nb.html)-->
Coming soon...

# Exercises for the adventurous
- Write a language for working with strings.
