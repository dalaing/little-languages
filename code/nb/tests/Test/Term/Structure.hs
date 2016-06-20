{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Term.Structure (
    structureTests
  ) where

-- from 'tasty'
import           Test.Tasty            (TestTree, testGroup)

-- from 'tasty-quickcheck'
import           Test.Tasty.QuickCheck (testProperty)

-- local
import           Term                  (contains, size, subTerms)
import           Term.Gen              (AnyTerm (..), ContainingTerm(..))

structureTests :: TestTree
structureTests =
  testGroup "structure"
    [ testProperty "contains" propContains
    , testProperty "subterms matches size" propSubtermsSize
    , testProperty "subterms matches contains" propSubtermsContains
    ]

propContains :: ContainingTerm
             -> Bool
propContains (ContainingTerm o i) =
  o == i || o `contains` i

propSubtermsSize :: AnyTerm
                 -> Bool
propSubtermsSize (AnyTerm tm) =
  size tm - 1 <= length (subTerms tm)

propSubtermsContains :: AnyTerm
                     -> Bool
propSubtermsContains (AnyTerm tm) =
  all (tm `contains`) (subTerms tm)
