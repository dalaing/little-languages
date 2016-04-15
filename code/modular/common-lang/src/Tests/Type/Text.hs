{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Tests.Type.Text (
    mkTextTests
  ) where

import           Control.Lens          (view)
import           Data.List             (group, intercalate, sort)
import           Test.QuickCheck       (Property, forAllShrink, property, (===))
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (Assertion, assertBool, testCase)
import           Test.Tasty.QuickCheck (testProperty)

import           Common.Parse          (parseFromString, ReservedWords(..))
import           Common.Pretty         (prettyToString)
import           Component             (ComponentOutput (..))
import           Component.Type.Gen    (HasGenTypeOutput (..))
import           Component.Type.Parse  (HasParseTypeOutput (..))
import           Component.Type.Pretty (HasPrettyTypeOutput (..))

mkTextTests :: ( Eq ty
               , Show ty
               )
            => ComponentOutput e ty tm a
            -> TestTree
mkTextTests c =
  testGroup "text"
    [ testCase "unique reserved words" $ assertUniqueReserved c
    , testProperty "unique parsing rule" $ propUniqueParse c
    , testProperty "pretty-parse round trip" $ propPrettyParse c
    ]

isRight :: Either a b
        -> Bool
isRight (Right _) =
  True
isRight _ =
  False

assertUniqueReserved :: ComponentOutput e ty tm a
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

propUniqueParse :: Show ty
                => ComponentOutput e ty tm a
                -> Property
propUniqueParse c =
  let
    genAnyType' = view genAnyType c
    shrAnyType' = view shrAnyType c
    prettyType' = view prettyType c
    parseTypeRules' = view parseTypeRules c
  in
    forAllShrink genAnyType' shrAnyType' $ \ty ->
    let
      text =
        prettyToString .
        prettyType' $
        ty
      matches =
        length .
        filter isRight .
        fmap (\p -> parseFromString p text) $
        parseTypeRules'
    in
      matches === 1

propPrettyParse :: ( Eq ty
                   , Show ty
                   )
                => ComponentOutput e ty tm a
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
  in
    forAllShrink genAnyType' shrAnyType' $ \ty ->
      case roundTrip ty of
        Left _ -> property False
        Right ty' -> ty === ty'
