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

import           Test.QuickCheck       (Property, forAllShrink, property, (===))
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Common.Parse          (parseFromString)
import           Common.Pretty         (prettyToString)
import           Term.Gen              (genTerm, shrinkTerm)
import           Term.Parse            (parseTerm, parseTermRules, withParens)
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

propUniqueParse :: Property
propUniqueParse =
  forAllShrink genTerm shrinkTerm $ \tm ->
    let
      text =
        prettyToString .
        prettyTerm $
        tm
      matches =
        length .
        filter isRight .
        fmap (\p -> parseFromString (withParens p) text) $
        parseTermRules
    in
      matches === 1

propPrettyParse :: Property
propPrettyParse =
  forAllShrink genTerm shrinkTerm $ \tm ->
    let
      roundTrip =
        parseFromString parseTerm .
        prettyToString .
        prettyTerm
    in
      case roundTrip tm of
        Left _ -> property False
        Right tm' -> tm === tm'
