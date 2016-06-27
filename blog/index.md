---
title: Home
---

# What's all this then?

I'm glad you asked.
It's a new blog series I'm writing, to go along with some code I've been hacking on and a talk I presented at YOW! Lambda Jam.

The focus is on writing Domain Specific Languages (DSLs) in Haskell.

Some of it will be about some of the cool tools, techniques and libraries we can use in Haskell.
There's some great stuff out there, with mixed amounts of documentation.
I'm not going to fully cover the gaps there, but hopefully I can get you to take a look at something you might otherwise have left on the shelf.

Some of it will be about how to get your feet wet with Programming Language Theory (PLT) in order to help improve your DSLs.
For the most part it's super useful, it's not anywhere near as scary as it looks going in, and you can get a lot of good use out of the easy bits that you pick up in the beginning.

Some of it will be madness, where I try to work out _if_ I can do something without much regard for _whether_ I should be doing it at all.
I'll try to remember to point out when I'm doing that.

There will also be some themes that run through this project: testability and composition.
We'll be using QuickCheck to do a lot of heavy lifting for us on the testing front.
For composition, we'll periodically refactor the code that we have to make things more reusable and composable.

# The talk

Here [is the video](https://www.youtube.com/watch?v=CZp9IFgFFts) and here are [the slides](https://github.com/dalaing/little-languages/raw/master/talks/ylj16/slides.pdf).

# The blog posts

## Part 1 - The basics

- [The B language](./posts/b.html)
    - [The semantics of B](./posts/b/semantics.html)
    - [Testing B](./posts/b/testing.html)
    - [Parsing and Printing B](./posts/b/text.html)
- [The N language](./posts/n.html)
- [The I language](./posts/i.html)
    - [Parsing and Printing I](./posts/i/text.html)
    - [Testing I](./posts/i/testing.html)

With many more on the way...

<!--
- [The NB language](./posts/nb.html)
    - [Testing NB](./posts/nb/testing.html)
    - [Adding annotations to the AST](./posts/nb/notes.html)
    - [Improving type errors](./posts/nb/typeerrors.html)
    - [Improving test generation](./posts/nb/improved-testing.html)
- [Simply-Typed Lambda Calculus](./posts/stlc.html)
    - [Using `bound` for variable substitution](./posts/stlc/bound.html) 
    - [Semantics for STLC-B](./posts/stlc/semantics.html)
    - [Adding contexts](./posts/stlc/contexts.html) 
    - [Types for STLC-B](./posts/stlc/types.html)
    - [Adding warnings](./posts/stlc/warnings.html) 

- LC and STLC
  - describe LC and it's various tricky cases in main post
  - describe bound
  - semantics of STLC, implementation using bound
  - type system for STLC, implementation using reader
  - add warnings for shadowing / unused variables

## Intermezzo 
  - lambda calculus cooked 5 ways
  - fun with hbound and syntax helpers
  - internal and external languages
  - small check (if we can turn sets of rules / structures into a maximum depth)

## Part 2 - Making things more modular

- modularity for NB (no annotations)
  - classy prisms for the term
    - rework the rules
  - a monoid for combining rules
  - typeclasses for combining rules
- adding annotations and LC
  - intro to constraints
  - the impact of extra type variables on the monoid approach
  - the impact of extra type variables on the typeclass approach

## Intermezzo
- observable sharing for CSE
  - transforming back from the graph into a let block could be interesting
- code formatters and more advanced testing of text functions

## Part 3 - Extensions
- let, fix, letrec
  - non-recursive let
  - adding fix for letrec
  - indentation aware parsing
- patterns
  - need source locations
- case / alt
  - pattern match coverage tests
- TLDs
- user defined data types
  - recast B using data types
- recursive data types
  - recast N using data types
  - recast NB using data types from the previous recastings
- a basic module system

## Intermezzo
- literate documents 
- a REPL for learning
  - from a REPL to a debugger
- a DSL for SQL 92 (?)

## Part 4 - Type inference
- constraint based type inference
- bidirectional type inference 
- System F, Fc, Fw
- Typeclasses (?) 

## Intermezzo
- strictness vs laziness via annotations type level variables
- affine languages

## Part 5 - Compilation of strict languages
- Tiger book approaches

## Part 5 - Compilation of lazy languages
- PJ and lester approaches
- Tiger book approaches

-->

# For those of you playing along at home

These posts should all be self-contained, so you probably don't _need_ the following books.
I had them in my books-that-I-want queue for a long time but wasn't sure I eas ready for them, and now I kind of wish that I dived in earlier.

There are two books on PLT that are standard recommendations.
The _reason_ they are standard recommendations is because they are great.

"Types and Programming Lanugages" (TAPL) by Benjamin Pierce.

[<img border="0" src="http://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0262162091&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=dlaingorg-20" />](http://www.amazon.com/gp/product/0262162091/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0262162091&linkCode=as2&tag=dlaingorg-20&linkId=NLSJ2WBLS67F5DD6)
<img src="http://ir-na.amazon-adsystem.com/e/ir?t=dlaingorg-20&l=as2&o=1&a=0262162091" width="1" height="1" border="0" alt="" style="border:none !important; margin:0px !important;" />


"Practical Foundations For Programming Languages" (PFPL) by Robert Harper.

[<img border="0" src="http://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1107150302&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=dlaingorg-20" />](https://www.amazon.com/Practical-Foundations-Programming-Languages-Robert/dp/1107150302/ref=as_li_ss_il?ie=UTF8&linkCode=li3&tag=dlaingorg-20&linkId=4d749d71d7a1fa79bee09a1d87583662)
<img src="https://ir-na.amazon-adsystem.com/e/ir?t=dlaingorg-20&l=li3&o=1&a=1107150302" width="1" height="1" border="0" alt="" style="border:none !important; margin:0px !important;" />

If it has been a while since you've done mathematical proofs, I highly recommend "How To Prove It" (HTPI) by Daniel Velleman.

[<img border="0" src="http://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0521675995&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=dlaingorg-20" />](http://www.amazon.com/gp/product/0521675995/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0521675995&linkCode=as2&tag=dlaingorg-20&linkId=HASLFK5VF7DN7YA7)
<img src="http://ir-na.amazon-adsystem.com/e/ir?t=dlaingorg-20&l=as2&o=1&a=0521675995" width="1" height="1" border="0" alt="" style="border:none !important; margin:0px !important;" />

The second chapter of TAPL mentions all of the math that you'll need to do all of the exercises. 
HTPI will get you there, and open some other doors as well.

An alternative recommendation is to work through "Software Foundations" in parallel with TAPL.
The first part teaches you how to use Coq to prove things.
After that it covers similar material to the first half of TAPL, with the Coq compiler checking your proofs along the way.

I'll recommend some more materials later on, when the content spirals off into some other directions, but for now the above should be fine.

# Why are you doing this?

When I was new to Haskell, someone mentioned a paper that covered something I was interested in, and so I started to skim it.

It was about an extension to Haskell, and I can clearly remember that it was formatted in two columns.
Other bits I'm a bit more vague about, since it was pretty alien to me.

The bit that really struck me was that it was all on one page.

The first column mentioned the additions to the type system in some syntax that I couldn't read.
The second column mentioned the addition to the semantics in some other-but-related syntax that I also couldn't read.
After the diagrams with the changes where the proofs that the changes didn't break anything in the existing system and that the changes delivered on their promises.

The paper then went on to discuss various extensions and alternatives to their change - clearly the authors had done what they'd wanted to do on that page, and were keen to discuss other matters.

I was very struck by how composable the two strange-looking syntaxes were, and how effective the existing proofs and proof strategies were at dealing with incremental changes.
That's when I asked some questions, picked up TAPL, and - after reading Chapter 2 of TAPL - picked up HTPI.

If you read a bit of TAPL or PFPL, you'll see that it takes a similar approach.
It spends a lot of time incrementally building up a series of languages and proving that the increments add what they claim to and that they don't make a mess while doing it.

I found that exciting, and hopefully I can spread some of that excitement around with this series.

There are also a few tools that make some of this easier, and I'm pretty keen to spread the word about them.
