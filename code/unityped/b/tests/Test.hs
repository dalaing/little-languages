module Main where

import Data.Maybe (isJust, catMaybes)

import Test.Tasty
import Test.Tasty.QuickCheck

import Term
import Term.Gen
import Term.Parse
import Term.Pretty
import Term.Eval.Value
import Term.Eval.SmallStep
import Term.Eval.BigStep

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
        testProperty "roundTrip" propRoundTrip
      , testProperty "smallUnique" propSmallUnique
      , testProperty "smallShrinks" propSmallShrinks
      , testProperty "bigUnique" propBigUnique
      , testProperty "smallBig" propSmallBig
      ]

propRoundTrip :: AnyTerm -> Property
propRoundTrip (AnyTerm t) =
  case (parseTermString . prettyString) t of
    Left _ -> property False
    Right u -> u === t

propSmallUnique :: AnyTerm -> Property
propSmallUnique (AnyTerm t) =
  (isJust . value $ t) .||.
  ((=== 1) . length . catMaybes . fmap ($ t) $ smallSteps)

propSmallShrinks :: AnyTerm -> Bool
propSmallShrinks (AnyTerm t) = case smallStep t of
  Nothing -> True
  Just u -> size u < size t

propBigUnique :: AnyTerm -> Property
propBigUnique (AnyTerm t) =
  (=== 1) . length . catMaybes . fmap ($ t) $ bigSteps

propSmallBig :: AnyTerm -> Property
propSmallBig (AnyTerm t) =
  sEval t === bEval t
