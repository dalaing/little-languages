module Test.Type.Text (
    textTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Type.Gen
import Type.Parse
import Type.Pretty

textTests :: TestTree
textTests =
  testGroup "text" [
    testProperty "roundTrip" propRoundTrip
  ]

propRoundTrip :: AnyType
              -> Property
propRoundTrip t =
    case roundTrip t of
      Left _ -> property False
      Right u -> u === t
  where
    roundTrip =
      fmap AnyType .
      parseTypeString .
      prettyTypeString .
      getAnyType
