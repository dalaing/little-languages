Things to do

- split tests out into groups for nb and nb-srcloc
  - terms vs types
    - print / parse
    - semantics
    - type checking
      - success / errors
        - probably some hunit for specific errors
        - quickcheck with gens with a single error, so we know what
          we're catching (if we ignore srclocs)

Get stlc-b working
  types now need srcloc info for type errors involving lambda terms
  need to check for free variables prior to type checking
    can move the last type variable to () after that, wrap and use the next type variable for type level stuff?

  add warnings for variable shadowing
  add warnings for unused variables

  add a pattern type
    - var, wild
    - capture location information about the patterns
    - type checker needs to know about patterns
  add a pattern list, to make binding for multi argument functions more efficient?
    - are there tricks we can do to make partial application simpler?

Get modularity working
  for stlc, b, n, i and various combinations
    ib -> eq, ord
    ni -> cast
    nb -> isZero

Unknown order

Add HM type inference to infer the types of lambda bindings
  - simplified because we don't have universals yet
  - criterion for different cases?

Add products and sums

Add TLDs with optional type annotations as something like a big let

Add user declared data types
  - TLD and pattern / type checker support
  - initially prevent recursive use
  - then add folds / unfolds to constructors / pattern matches
    - and the isorecursive type / semantic rules
    - relax the restriction on recursive use
    
Add LLVM output
  - strict semantics
  - good testing

Add primitives
  - use primitives when they match default LC semantics
  - marking segments of code as strict or non-strict could be interesting
    - in which case different primitives could mark themselves as strict or non-strict
    - could have strictness variables, so that you be marked as strict, non-strict, or var s, 
      such that s matches up with the strictness in code you're composing with, and eventually gets 
      unified to something

Add universals

Add typeclasses, or at least explore the space
