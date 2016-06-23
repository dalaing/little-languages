{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Type.Text (
    mkTextTests
  ) where

import           Data.List               (group, intercalate, sort)

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
import           Component.Type.Gen      (HasGenTypeOutput (..))
import           Component.Type.Parse    (HasParseTypeOutput (..))
import           Component.Type.Pretty   (HasPrettyTypeOutput (..))
import Extras (Eq1(..), Show1(..))

mkTextTests :: ( Eq1 ty
               , Show1 ty
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
    ReservedWords w1 w2 w3 = view typeReservedWords c
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
                     Eq1 ty
                   , Show1 ty
                   )
                => ComponentOutput r e ty tm
                -> Property
propPrettyParse c =
  let
    genAnyType' = view genAnyType c
    shrAnyType' = view shrAnyType c
    prettyType' = view prettyType c
    parseType' = view parseType c
    roundTrip =
      parseFromString parseType' .
      prettyToString .
      prettyType'
    test ty =
      case roundTrip ty of
        Left _ -> property False
        Right ty' -> ty === ty'
          \\ (spanEq1 :: Eq Span :- Eq (ty Span))
          \\ (spanShow1 :: Show Span :- Show (ty Span))
  in
    forAllShrink genAnyType' shrAnyType' test
      \\ (spanShow1 :: Show Span :- Show (ty Span))
