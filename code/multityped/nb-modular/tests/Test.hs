module Main where

import Control.Lens (view)
import Test.Tasty

import Common (tests)
import Language.NB

main :: IO ()
main =
  defaultMain .
  view tests $
  languageOutput
