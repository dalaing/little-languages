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
import           Test.QuickCheck       (Property, forAllShrink, property, (===))

-- local
import           Common.Parse          (parseFromString)
import           Common.Pretty         (prettyToString)
import           Term.Gen              (genTerm, shrinkTerm)
import           Term.Parse            (parseTerm, parseTermRules)
import           Term.Pretty           (prettyTerm)

textTests :: TestTree
textTests = testGroup "text"
  [ testProperty "pretty-parse round trip" propPrettyParse
  ]

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
