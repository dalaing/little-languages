{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Term.Text (
    mkTextTests
  ) where

import           Data.List               (group, intercalate, sort)
import Data.Proxy (Proxy)

import           Control.Lens            (view)
import           Test.QuickCheck         (Property, forAllShrink, property,
                                          (===))
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (Assertion, assertBool, testCase)
import           Test.Tasty.QuickCheck   (testProperty)
import           Text.Trifecta.Rendering (Span)
import Data.Constraint

import           Common.Parse            (ReservedWords (..), parseFromString)
import           Common.Pretty           (prettyToString)
import           Component               (ComponentOutput (..))
import           Component.Term.Gen      (HasGenTermOutput (..))
import           Component.Term.Parse    (HasParseTermOutput (..))
import           Component.Term.Pretty   (HasPrettyTermOutput (..))
import Extras (Eq3(..), Show3(..))

mkTextTests :: ( Eq3 tm
               , Show3 tm
               )
            => ComponentOutput r e ty tm
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

assertUniqueReserved :: ComponentOutput r e ty tm
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

propPrettyParse :: forall r e ty tm. (
                     Eq3 tm
                   , Show3 tm
                   )
                => ComponentOutput r e ty tm
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
    test :: tm Span Span String -> Property
    test tm =
      case roundTrip tm of
        Left _ -> property False
        Right tm' -> tm === tm'
          \\ (spanEq3 :: (Eq Span, Eq Span, Eq String) :- Eq (tm Span Span String))
          \\ (spanShow3 :: (Show Span, Show Span, Show String) :- Show (tm Span Span String))
  in
    forAllShrink genAnyTerm' shrAnyTerm' test
      \\ (spanShow3 :: (Show Span, Show Span, Show String) :- Show (tm Span Span String))
