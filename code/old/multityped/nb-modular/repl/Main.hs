module Main where

import Control.Lens (view)

import Common (repl)
import Language.NB

main :: IO ()
main =
  view repl languageOutput
