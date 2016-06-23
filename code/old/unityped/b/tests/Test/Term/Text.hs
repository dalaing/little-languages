module Test.Term.Text where

import Test.Tasty
import Test.Tasty.QuickCheck

import Term.Gen
import Term.Parse
import Term.Pretty

textTests :: TestTree
textTests =
  testGroup "text" [
    testProperty "roundTrip" propRoundTrip
  ]

propRoundTrip :: AnyTerm
              -> Property
propRoundTrip (AnyTerm t) =
    case roundTrip t of
      Left _ -> property False
      Right u -> u === t
  where
    roundTrip =
      parseTermString .
      prettyString
