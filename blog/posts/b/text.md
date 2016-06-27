---
title: Printing and Parsing *B*
published: 2016-06-27 12:00:00+10:00
---

[Previously](testing.html) we wrote some `QuickCheck` to test the evaluation of *B*.
Now we want to be able to more easily play around with it.

# Parsing *B*

We'll be using `parsers` for our parsing needs, which is compatible with `parsec`, `attoparsec` and `trifecta`.
If you're familiar with `parsec` then you should be fine, although we'll be driving it via `trifecta`.

We'll use this is a little helper to test our parsers:
```haskell
import Text.Trifecta.Parser
import Text.Trifecta.Result
import Text.Trifecta.Delta
import Text.PrettyPrint.ANSI.Leijen

parse :: Parser a -> String -> Result a
parse p s = case parseString p (Lines 0 0 0 0) s of
  Failure d -> Failure (plain d)
  Success s -> Success s
```

I'm going to amend the `Term` type for this section, so that I can better demonstrate a few things about `parsers`.
The change is to add variables into the mix:
```haskell
data Term =
    TmVar String
  | TmFalse
  | TmTrue
  | TmIf Term Term Term
  deriving (Eq, Ord, Show)
```

## CharParsing

We'll start off using the `CharParsing` class, which is used for parsing character streams.

Variables will start with a lowercase letter, followed by some number (possibly 0) of alpha-numeric characters or underscores.
```haskell
parseTmVar :: (Monad m, CharParsing m) 
           => m Term
parseTmVar =
  (\x y -> TmVar (x : y)) <$> 
    lower <*> 
    many (alphaNum <|> char '_')
```

We'll use capital letters for our Boolean constructors: 
```haskell
parseTmTrue :: (Monad m, CharParsing m) 
            => m Term
parseTmTrue =
  TmTrue <$ 
    string "True"

parseTmFalse :: (Monad m, CharParsing m) 
             => m Term
parseTmFalse =
  TmFalse <$ 
    string "False"
```
and we'll follow convention for the if-expression:
```haskell
parseTmIf :: (Monad m, CharParsing m) 
          => m Term 
          -> m Term
parseTmIf parse =
  TmIf <$
    string "if"   <* spaces <*> parse <* spaces <*
    string "then" <* spaces <*> parse <* spaces <*
    string "else" <* spaces <*> parse
```
where we pass in the combined parsing function as an argument.

`CharParsing` has `Alternative` as an indirect superclass, so we can combine these rules directly with `asum`.
While we're here, we'll also add support for parsing terms that are in parentheses:
```haskell
parseTermRules :: (Monad m, CharParsing m) 
               => [m Term]
parseTermRules =
  [ parseTmVar
  , parseTmTrue
  , parseTmFalse
  , parseTmIf parseTerm
  ]
  
parens :: (Monad m, CharParsing m) 
       => m a 
       -> m a
parens =
  between (char '(' <* spaces) (spaces <* char ')')
  
parseTerm :: (Monad m, CharParsing m) 
          => m Term
parseTerm = 
  asum parseTermRules <|> 
  parens parseTerm
```

Now we'll try it on a few things.

```haskell
> parse parseTerm "False"
Success TmFalse
```

```haskell
> parse parseTerm "potato"
Success (TmVar "potato")
```

Looking good so far.

```haskell
> parse parseTerm "if False then False else True"
Success (TmVar "if")
```

Ouch.
It turns out that order that we specify our rules matters here.

Let's fix that:
```haskell
parseTermRules :: (Monad m, CharParsing m) 
               => [m Term]
parseTermRules =
  [ parseTmTrue
  , parseTmFalse
  , parseTmIf parseTerm
  , parseTmVar
  ]
```

Now we should be able to parse if-expressions:
```haskell
> parse parseTerm "if False then False else True"
Success (TmIf TmFalse TmFalse TmTrue)
```
and no potatoes were harmed during the fix:
```haskell
> parse parseTerm "potato"
Success (TmVar "potato")
```

However, there is room for improvement:
```haskell
> parse parseTerm "Potato"
Failure (interactive):1:1: error: expected: "(",
    "False", "True", "if",
    lowercase letter
Potato<EOF>
^
```

That's a bit ugly, and the ugliness can be addressed by giving names to each of our parsers.

We do this with the `<?>` operator, which has an operator precedence of 0:
```haskell
parseTmVar :: (Monad m, CharParsing m) 
           => m Term
parseTmVar =
  (\x y -> TmVar (x : y)) <$> 
    lower <*> 
    many (alphaNum <|> char '_')
  <?> "Var"

parseTmTrue :: (Monad m, CharParsing m) 
            => m Term
parseTmTrue =
  TmTrue <$ 
    string "True"
    <?> "True"

parseTmFalse :: (Monad m, CharParsing m) 
             => m Term
parseTmFalse =
  TmFalse <$ 
    string "False"
    <?> "False"

parseTmIf :: (Monad m, CharParsing m) 
          => m Term 
          -> m Term
parseTmIf parse =
  TmIf <$
    string "if"   <* spaces <*> parse <* spaces <*
    string "then" <* spaces <*> parse <* spaces <*
    string "else" <* spaces <*> parse
    <?> "if-then-else"
  
parseTerm :: (Monad m, CharParsing m) 
          => m Term
parseTerm = 
  asum parseTermRules <|> 
  parens parseTerm
  <?> "term"
```

Now our error message makes more sense:
```haskell
> parse parseTerm "Potato"
Failure (interactive):1:1: error: expected: term
Potato<EOF>
^
```
aside from the recurring presence of potatoes in our examples.

Alas, our work is not done, since we'd like this to be an error: 
```haskell
> parse parseTerm "if potato thenFalseelseTrue"
Success (TmIf (TmVar "potato") TmFalse TmTrue)
```

We could go and fiddle with our parsers, but there's a quicker way.

## TokenParsing

We'll step things up a little by using the `TokenParsing` class, which is used for parsing token streams.
This provides a few benefits - the first of which being that we don't have to worry about whitespace as much.

For our identifiers, we just wrap our character-based parser up with `token`:
```haskell
parseTmVar :: (Monad m, TokenParsing m) 
           => m Term
parseTmVar =
  (token (\x y -> TmVar (x : y)) <$> 
    lower <*> 
    many (alphaNum <|> char '_'))
  <?> "Var"
```

For the other constructors we can just use `symbol` to parse a literal string with whitespace around it: 
```haskell
parseTmTrue :: (Monad m, TokenParsing m)
            => m Term
parseTmTrue =
  TmTrue <$ 
    symbol "True"
    <?> "True"

parseTmFalse :: (Monad m, CharParsing m) 
             => m Term
parseTmFalse =
  TmFalse <$ 
    symbol "False"
    <?> "False"

parseTmIf :: (Monad m, TokenParsing m) 
          => m Term 
          -> m Term
parseTmIf parse =
  TmIf <$
    symbol "if"   <* parse <*
    symbol "then" <* parse <*
    symbol "else" <* parse
    <?> "if-then-else"
```

We still have our list of rules:
```haskell
parseTermRules :: (Monad m, TokenParsing m) 
               => [m Term]
parseTermRules =
  [ parseTmVar
  , parseTmTrue
  , parseTmFalse
  , parseTmIf parseTerm
  ]
``` 

But now we can use the whitespace-aware `parens` from `Text.Parser.Token`:
```haskell
parseTerm :: (Monad m, TokenParsing m) 
          => m Term
parseTerm = 
  asum parseTermRules <|> 
  parens parseTerm
  <?> "term"
```

This fixes our whitespace problems:
```haskell
> parse parseTerm "if potato then FalseelseTrue"'
Failure (interactive):1:16: error: expected: term
    if potato then FalseelseTrue<EOF>
                   ^
```
but we still need to be careful with the order of our rules.

## Token parsing with style

The `TokenParsing` class has support for something like lexing, by way of various `IdentifierStyle`s.

The style has
- a name
- a parser for the first character
- a parser for characters
- a set of reserved words
- highlighting hints for regular words and reserved words

From this we can extract a parser for reserved words, and a parser for regular words that won't accept the reserved words.
The highlighting comes into play later on, when we mine `trifecta` for all the prettiness we can get.

We gleefully exploit this by setting up styles for our the two semantic classes that we have - identifiers and constructors - and reserving our keywords in whichever styles are able to conflict with them.

For example, since "if", "then" and "else" could conflict with our variables, we reserve them in the indentifier style:
```haskell
import qualified HashSet as HS

identifierStyle :: TokenParsing m
                => IdentifierStyle m
identifierStyle =
    IdentifierStyle {
      _styleName              = "identifier"
    , _styleStart             = lower <|> char '_'
    , _styleLetter            = alphaNum <|> char '_'
    , _styleReserved          = HS.fromList reservedIdentifiers
    , _styleHighlight         = Identifier
    , _styleReservedHighlight = ReservedIdentifier
  }
  where
    reservedIdentifiers =
      [ "if"
      , "then"
      , "else"
      ]
```

We make use of the style via `reserve` and `ident`, to create a pair of helpers:
```haskell
reservedIdentifier :: (Monad m, TokenParsing m)
                   => String
                   -> m ()
reservedIdentifier =
  reserve identifierStyle

identifier :: (Monad m, TokenParsing m)
           => m String
identifier =
  ident identifierStyle
```

We do something similar for constructors.
In this case it feels less exploitative, since 'True' and 'False' are genuinely constructors.
```haskell
constructorStyle :: TokenParsing m
                 => IdentifierStyle m
constructorStyle =
    IdentifierStyle {
      _styleName              = "constructor"
    , _styleStart             = upper
    , _styleLetter            = alphaNum <|> char '_'
    , _styleReserved          = HS.fromList reservedConstructors
    , _styleHighlight         = Constructor
    , _styleReservedHighlight = ReservedConstructor
    }
  where
    reservedConstructors =
      [ "False"
      , "True"
      ]
```
with the corresponding helpers:
```haskell
reservedConstructor :: (Monad m, TokenParsing m)
                    => String
                    -> m ()
reservedConstructor =
  reserve constructorStyle

constructor :: (Monad m, TokenParsing m)
            => m String
constructor =
  ident constructorStyle
```

Now that we have that out of the way, we can use those styles with the parsers for `Term`:
```haskell
parseTmVar :: (Monad m, TokenParsing m) 
           => m Term
parseTmVar =
  TmVar <$> identifier <?> "Var"

parseTmTrue :: (Monad m, TokenParsing m)
            => m Term
parseTmTrue =
  TmTrue <$ 
    reservedConstructor "True"
    <?> "True"

parseTmFalse :: (Monad m, CharParsing m) 
             => m Term
parseTmFalse =
  TmFalse <$ 
    reservedConstructor "False"
    <?> "False"

parseTmIf :: (Monad m, TokenParsing m) 
          => m Term 
          -> m Term
parseTmIf parse =
  TmIf <$
    reservedIdentifier "if"   <* parse <*
    reservedIdentifier "then" <* parse <*
    reservedIdentifier "else" <* parse
    <?> "if-then-else"
```
and now we don't have to be wary of the order in which we combine the parsing rules.

# Pretty Printing *B*

We'll use `ansi-wl-pprint` for our pretty printing needs.

Pretty printing libraries give us the ability to turn our structured data into text, with abstractions for controlling the layout of the text and nice compositional properties.

Pretty printing libraries have some similarity to combinator based parsing libraries - they're both easy once they click, but there can be some stumbling blocks when you're new to them.
It's worth spending some time to get used to a pretty printing library, so that you can use it at a moments notice without having to stumble.

There are many pretty printing libraries out there, but I've reached for `ansi-wl-pprint`.
It uses a fairly common set of abstractions for pretty printing, and it has support for coloured output.
It's also used by `trifecta`, which we abuse shortly.

The main data type in this library is the abstract document, `Doc`.

There is a `Show` instance for `Doc`, or we can use `putDoc` to pretty print the `Doc` with colourisation.

We can convert `String`s to `Doc`s:
```haskell
> text "potato"
potato
```

And, although the syntax highlighting of my blog isn't going to play along, we can alter that in various ways
```haskell
> green . text $ "potato"
potato
> bold . green . text $ "potato"
potato
> underline . bold . green . text $ "potato"
potato
```

We can also set the number of columns in the display and the ribbon width, which is the number of non-indentation characters per line.

It can be useful to play around with this to get a feel for it.
I'll be using this little function for my experiments, which allows me to set the number of columns and ribbon width:
```haskell
render :: Float -> Int -> (a -> Doc) -> a -> IO ()
render r w f d = putStr $ displayS (renderPretty r w (plain (f d))) ""
```

In this function, `w` is the number of columns to use, and `r` * `w` is the ribbon width.
We also uses `plain` to strip the various escape codes for colourisation, which I do so I can use it for doctests.

We're going to proceed with rule-based pretty printers for each of our terms, and lean on the `Alternative` instance of `Maybe` to stitch them all together.

The cases for `TmTrue` and `TmFalse` are straightforward:
```haskell
prettyTmTrue :: Term
             -> Maybe Doc
prettyTmTrue TmTrue =
  Just $ text "True"
prettyTmTrue _ =
  Nothing

prettyTmFalse :: Term
              -> Maybe Doc
prettyTmFalse TmFalse =
  Just $ text "False"
prettyTmFalse _ =
  Nothing
```

For the `TmIf` case we use a few more combinators.
We use `<+>` for horizontal concatenation with a space.
We use `</>` for soft line breaks, which will become a space if the combined arguments fit the page with, and a new line if they don't.

We pass in the combined pretty printer for terms in order to pretty print the sub-terms:
```haskell
prettyTmIf :: (Term -> Doc)
           -> Term
           -> Maybe Doc
prettyTmIf prettyTerm (TmIf tm1 tm2 tm3) =
  Just $
    text "if" <+> prettyTerm tm1 </>
    text "then" <+> prettyTerm tm2 </>
    text "else" <+> prettyTerm tm3
prettyTmIf _ _ =
  Nothing
```

After that we gather up the rules:
```haskell
prettyTermRules :: [Term -> Maybe Doc]
prettyTermRules =
  [ prettyTmFalse
  , prettyTmTrue
  , prettyTmIf prettyTerm
  ]
```
and combine them all:
```haskell
prettyTerm :: Term
           -> Doc
prettyTerm tm =
  fromMaybe (text "???") .
  asum .
  fmap ($ tm) $
  prettyTermRules
```
and we have our pretty printer.

(If we every see a "???", then things have gone pretty badly.)

We can play around with this to see how it looks.

```haskell
> render 0.5 40 prettyTerm $ TmTrue
True

> render 0.5 40 prettyTerm $ TmFalse
False
```

Looking good so far.

```haskell
> let ifTerm = TmIf TmFalse TmFalse TmTrue

> render 0.25 40 prettyTerm ifTerm
if False
then False
else True

> render 0.50 40 prettyTerm ifTerm
if False then False
else True

> render 0.75 40 prettyTerm ifTerm
if False then False else True
```

Hmm.  I'm not 100% sold on the middle one of those.

```haskell
> let nestedIfTerm = TmIf ifTerm ifTerm ifTerm

> render 0.25 40 prettyTerm nestedIfTerm 
if if False
then False
else True
then if False
then False
else True
else if True
then False
else True

> render 0.50 40 prettyTerm nestedIfTerm 
if if False
then False else True
then if False
then False else True
else if True
then False else True

> render 0.75 40 prettyTerm nestedIfTerm 
if if False then False
else True then if False
then False else True
else if True then False
else True
```

Aargh! My eyes!  I thought this was meant to be _pretty_!

## Improving the layout

For the first of those examples, we can improve things by changing from soft line breaks to hard line breaks.
```haskell
import Text.PrettyPrint.ANSI.Leijen as PP ((<$>))

prettyTmIf :: (Term -> Doc)
           -> Term
           -> Maybe Doc
prettyTmIf prettyTerm (TmIf tm1 tm2 tm3) =
  Just $
    text "if" <+> prettyTerm tm1 PP.<$>
    text "then" <+> prettyTerm tm2 PP.<$>
    text "else" <+> prettyTerm tm3
prettyTmIf _ _ =
  Nothing
```

Note that several pretty printing libraries use `<$>` for vertical composition, so there's usually some importing hoops to jump through so that you'd don't clash with the operator form of `fmap`.

Now we have:
```haskell
> let ifTerm = TmIf TmFalse TmFalse TmTrue

> render 0.25 40 prettyTerm ifTerm
if False
then False
else True

> render 0.50 40 prettyTerm ifTerm
if False 
then False
else True

> render 0.75 40 prettyTerm ifTerm
if False 
then False 
else True
```
which looks a bit neater, but it's using a lot of vertical space that it doesn't need to.

On top of that, this:
```haskell
> render 0.25 40 prettyTerm nestedIfTerm 
if if False
then False
else True
then if False
then False
else True
else if True
then False
else True

> render 0.50 40 prettyTerm nestedIfTerm 
if if False
then False
else True
then if False
then False
else True
else if True
then False
else True

> render 0.75 40 prettyTerm nestedIfTerm 
if if False
then False
else True
then if False
then False
else True
else if True
then False
else True
```
is still an abomination.

We can improve things for the nested if-expression using `align`.
When `align d` is used, all new lines in `d` start on the same column as the first column of `align d`.
```haskell
prettyTmIf :: (Term -> Doc)
           -> Term
           -> Maybe Doc
prettyTmIf prettyTerm (TmIf tm1 tm2 tm3) =
  Just . align $
    text "if" <+> prettyTerm tm1 PP.<$>
    text "then" <+> prettyTerm tm2 PP.<$>
    text "else" <+> prettyTerm tm3
prettyTmIf _ _ =
  Nothing
```

This neatens things up somewhat:
```haskell
>>> render 0.25 40 prettyTerm nestedIfTerm
if if False
   then False
   else True
then if False
     then False
     else True
else if True
     then False
     else True

>>> render 0.50 40 prettyTerm nestedIfTerm
if if False
   then False
   else True
then if False
     then False
     else True
else if True
     then False
     else True

>>> render 0.75 40 prettyTerm nestedIfTerm
if if False
   then False
   else True
then if False
     then False
     else True
else if True
     then False
     else True
```

Finally, we can improve the use of vertical space with `group`.
When `group d` is used, it attempts removes all of the linebreaks in `d`.
If that fits on the current line it is used, otherwise it falls back to `d`.
```haskell
prettyTmIf :: (Term -> Doc)
           -> Term
           -> Maybe Doc
prettyTmIf prettyTerm (TmIf tm1 tm2 tm3) =
  Just . group . align $
    text "if" <+> prettyTerm tm1 PP.<$>
    text "then" <+> prettyTerm tm2 PP.<$>
    text "else" <+> prettyTerm tm3
prettyTmIf _ _ =
  Nothing
```

We can see the effect on the simple if-expression here:
```haskell
>>> render 0.25 40 prettyTerm ifTerm
if False
then False
else True

>>> render 0.50 40 prettyTerm ifTerm
if False
then False
else True

>>> render 0.75 40 prettyTerm ifTerm
if False then False else True
```

The selection of combinators in `ansi-wl-pprint` is pretty versatile, so we could keep fiddling with this for ages.

I'm going to stop here, but it's worth spending some time having a look at what it offers and having a play around with it.

## Token printing

Token printing isn't really a thing, but it's handy to be able to think of your parsing and printing as being closely related.

For now, we're just going to steal the highlighting from `trifecta` so that the parser and printer agree, and we'll use the same names for our helper functions for different semantic groups of things in both the parser and the printer.

As well as helping make things consistently pretty, this helps set the stage for bidirectional parsing and printing later on.

We import a few things to effect our heist:
```haskell
import           Text.Parser.Token.Highlight  (Highlight (..))
import           Text.PrettyPrint.ANSI.Leijen (Doc, text)
import           Text.Trifecta.Highlight      (withHighlight)

reservedIdentifier :: String
                   -> Doc
reservedIdentifier =
  withHighlight ReservedIdentifier .
  text

identifier :: String
           -> Doc
identifier =
  withHighlight Identifier .
  text

reservedConstructor :: String
                    -> Doc
reservedConstructor =
  withHighlight ReservedConstructor .
  text

constructor :: String
            -> Doc
constructor =
  withHighlight Constructor .
  text
```
and then we spend our loot to update our pretty printers:
```haskell
prettyTmFalse :: Term
              -> Maybe Doc
prettyTmFalse TmFalse =
  Just $ reservedConstructor "False"
prettyTmFalse _ =
  Nothing

prettyTmTrue :: Term
             -> Maybe Doc
prettyTmTrue TmTrue =
  Just $ reservedConstructor "True"
prettyTmTrue _ =
  Nothing

prettyTmIf :: (Term -> Doc)
           -> Term
           -> Maybe Doc
prettyTmIf prettyTerm (TmIf tm1 tm2 tm3) =
  Just . group . align $
    reservedIdentifier "if" <+> prettyTerm tm1 PP.<$>
    reservedIdentifier "then" <+> prettyTerm tm2 PP.<$>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIf _ _ =
  Nothing
```

# Testing the parser and the printer

We can use a few little helper functions:
```haskell
prettyToString :: Doc -> String
prettyToString d =
  displayS (renderPretty 0.75 80 (plain d)) ""

parseFromString :: Parser a
                -> String
                -> Either Doc a
parseFromString p s =
  case parseString p (Lines 0 0 0 0) s of
    Success r -> Right r
    Failure d -> Left d
```
to check that pretty printing a term and parsing the resulting the string always gives us back the original term:
```haskell
propPrettyParse :: AnyTerm -> Property
propPrettyParse (AnyTerm tm) =
  let
    roundTrip =
      parseFromString parseTerm .
      prettyToString .
      prettyTerm
  in
    case roundTrip tm of
      Left _ -> property False
      Right tm' -> tm === tm'
```

Tests like this can give us quite a bit of confidence in the correct paths, but it doesn't tell us much about our parsing errors.

## Doctests are your friend

This is one of the places where I like to use doctests.

They're great as unit tests, they're great as examples in documentation, and if you make them part of your build step there's minimal hassle involved in keeping them up to date.

Here's the haddock for `parseTmIf`:
```haskell
-- | A parser for 'TmIf'.
--
-- This parser should handle if-expressions:
-- >>> parse (parseTmIf parseTerm) "if False then False else True"
-- Success (TmIf TmFalse TmFalse TmTrue)
--
-- The sub-terms have to parse successfully:
-- >>> parse (parseTmIf parseTerm) "if potato then False else True"
-- Failure (interactive):1:4: error: expected: term
-- if potato then False else True<EOF>
--    ^
--
-- and we need to use the right keywords in the right spots:
-- >>> parse (parseTmIf parseTerm) "if False potato False else True"
-- Failure (interactive):1:10: error: expected: "then"
-- if False potato False else True<EOF>
--          ^
-- 
-- We also can't get away with truncating the expression:
-- >>> parse (parseTmIf parseTerm) "if False then False else"
-- Failure (interactive):1:25: error: unexpected
--     EOF, expected: end of "else",
--     term
-- if False then False else<EOF>
--                         ^
--
-- >>> parse (parseTmIf parseTerm) "if False then False"
-- Failure (interactive):1:20: error: unexpected
--     EOF, expected: "else",
--     end of "False"
-- if False then False<EOF>
--                    ^
--
```

In this case I'm using doctests for unit testing, so I have a test suite for doctests in my .cabal file.

I also have two modules for different approaches to parsing and printing, which collect the various approaches and capture their outputs in doctests.
This is mostly in case I end up forgetting why I did something, but it's been handy for blogging as well.

Doctests are also pretty good for a quick and dirty version of test-driven development.
The next time pretty printers turn up in this series, we'll be using a function that had my scratching my head for a while.
Eventually I wrote all of the doctests that I wanted and started coding against them.

# A REPL for *B*

We now have everything that we need for a read-eval-print-loop (REPL).

We'll use `haskeline` for this.

It's a Haskell version of readline, so it's just we want for a REPL.
This gives us some very nice things, including the ability to use up and down to move through history, Ctrl-R and Ctrl-S to search through the history, and Ctrl-A and Ctrl-E to go to the start or end of the line.

It has support for emacs or vim bindings, persistent history and command completion, so we can easily get quite a bit fancier if we want to.

For now we're going to start slowly.

Haskeline programs run inside the `InputT` monad transformer.
We use `runInputT` with `defaultSettings` to kick things off:
```haskell
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
       ... -- here is where our loop goes
       loop
```

From there, we add reading and printing lines:
```haskell
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      i <- getInputLine "> "
      case i of
        Nothing -> return ()
        Just "quit" -> return ()
        Just i' -> do
          o <- parseAndEval i' -- we still need to supply parseAndEval
          outputStrLn o
       loop
```
We can quit the loop with Ctrl-D on a blank line, in which case `getInputLine` returns `Nothing`.

We're going to write the function:
```haskell
parseAndEval :: String
             -> Doc
```
but in order to get the pretty colours from `ansi-wl-pprint` we need to use `putDoc`, which has the type `Doc -> IO ()`.

To do that, we remember that `InputT` has a `MonadIO` instance, and replace `outputStrLn` with `liftIO . putDoc . (<> line)`:
```haskell
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      i <- getInputLine "> "
      case i of
        Nothing -> return ()
        Just "quit" -> return ()
        Just i' -> do
          liftIO . putDoc . (<> line) . parseAndEval $ i'
          loop
```

Now we just have to write `parseAndEval`, which is a pure function.
The parser from `trifecta` reports errors using the `Doc` type from `ansi-wl-pprint`, so that makes our task much easier.

Once we've realised that, all we really need to do is parse the string and then evaluate the resulting term:
```haskell
parseAndEval :: String
             -> Doc
parseAndEval s =
  case parse parseTerm s of
    Left err -> err
    Right tm ->
       prettyTerm tm <+> 
       text "==>" <+> 
       prettyTerm (eval tm)
```

Now we have our first REPL!

# There has to be more than *B*

At this point we have some familiarity with the basic pieces we'll be using when working with language.

We're going to step things up to include languages with multiple types shortly, but before then we're going to look at a few more uni-typed languages in order to fill up our bag of tricks.

To begin with, we'll look at *N*, the language of natural numbers.

[Read on!](../n.html)

# Exercises for the adventurous 
- `trifecta` can gather source locations as it parses.  Add source annotations to `Term` and change the parser to annotate terms with the source locations of all of their sub-terms.
    - Don't panic if you struggle with this - I'll be covering it in one of the next few posts.
