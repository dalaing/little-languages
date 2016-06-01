{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Test.Type.Text (
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
import           Type.Gen              (genType, shrinkType)
import           Type.Parse            (parseType, parseTypeRules)
import           Type.Pretty           (prettyType)

textTests :: TestTree
textTests = testGroup "text"
  [ testProperty "unique parsing rule" propUniqueParse
  , testProperty "pretty-parse round trip" propPrettyParse
  ]

propUniqueParse :: Property
propUniqueParse =
  forAllShrink genType shrinkType $ \ty ->
    let
      isRight (Right _) = True
      isRight _ = False
      text =
        prettyToString .
        prettyType $
        ty
      matches =
        length .
        filter isRight .
        fmap (\p -> parseFromString p text) $
        parseTypeRules
    in
      matches === 1

propPrettyParse :: Property
propPrettyParse =
  forAllShrink genType shrinkType $ \ty ->
    let
      roundTrip =
        parseFromString parseType .
        prettyToString .
        prettyType
    in
      case roundTrip ty of
        Left _ -> property False
        Right ty' -> ty === ty'
