{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Term.Text (
    mkTextTests
  ) where

import           Control.Lens            (view)
import           Data.List               (group, intercalate, sort)
import           Test.QuickCheck         (Property, forAllShrink, property,
                                          (===))
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (Assertion, assertBool, testCase)
import           Test.Tasty.QuickCheck   (testProperty)
import           Text.Trifecta.Rendering (Span)

import           Common.Parse            (ReservedWords (..), parseFromString)
import           Common.Pretty           (prettyToString)
import           Component               (ComponentOutput (..))
import           Component.Term.Gen      (HasGenTermOutput (..))
import           Component.Term.Parse    (HasParseTermOutput (..))
import           Component.Term.Pretty   (HasPrettyTermOutput (..))

mkTextTests :: ( Eq (tm Span Span String)
               , Show (tm Span Span String)
               )
            => ComponentOutput r e ty Span tm Span String
            -> TestTree
mkTextTests c =
  testGroup "text"
    [ testCase "unique reserved words" $ assertUniqueReserved c
    , testProperty "pretty-parse round trip" $ propPrettyParse c
    ]

isRight :: Either a b
        -> Bool
isRight (Right _) =
  True
isRight _ =
  False

assertUniqueReserved :: ComponentOutput r e ty nTy tm nTm a
                     -> Assertion
assertUniqueReserved c =
  let
    ReservedWords w1 w2 w3 = view termReservedWords c
    w = w1 ++ w2 ++ w3
    duplicates =
      filter ((/= 1) . length) .
      group .
      sort $
      w
    msg = "Reserved word collisions: " ++
      intercalate "," (fmap head duplicates)
    unique =
      null duplicates
  in
    assertBool msg unique

propPrettyParse :: ( Eq (tm Span Span String)
                   , Show (tm Span Span String)
                   )
                => ComponentOutput r e ty Span tm Span String
                -> Property
propPrettyParse c =
  let
    genAnyTerm' = view genAnyTerm c
    shrAnyTerm' = view shrAnyTerm c
    prettyTerm' = view prettyTerm c
    parseTerm' = view parseTerm c
    roundTrip =
      parseFromString parseTerm' .
      prettyToString .
      prettyTerm'
  in
    forAllShrink genAnyTerm' shrAnyTerm' $ \tm ->
      case roundTrip tm of
        Left _ -> property False
        Right tm' -> tm === tm'
