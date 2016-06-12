{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Term.Text (
    textTests
  ) where

-- from 'tasty'
import           Test.Tasty            (TestTree, testGroup)

-- from 'tasty-quickcheck'
import           Test.Tasty.QuickCheck (testProperty)

-- from 'QuickCheck'
import           Test.QuickCheck       (Property, property, (===))

-- local
import           Common.Parse          (parseFromString)
import           Common.Pretty         (prettyToString)
import           Term.Gen              (AnyTerm(..))
import           Term.Parse            (parseTerm, parseTermRules)
import           Term.Pretty           (prettyTerm)

textTests :: TestTree
textTests = testGroup "text"
  [ testProperty "unique parsing rule" propUniqueParse
  , testProperty "pretty-parse round trip" propPrettyParse
  ]

isRight :: Either a b
        -> Bool
isRight (Right _) =
  True
isRight _ =
  False

propUniqueParse :: AnyTerm
                -> Property
propUniqueParse (AnyTerm tm) =
  let
    text =
      prettyToString .
      prettyTerm $
      tm
    matches =
      length .
      filter isRight .
      fmap (\p -> parseFromString p text) $
      parseTermRules
  in
    matches === 1

propPrettyParse :: AnyTerm
                -> Property
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
