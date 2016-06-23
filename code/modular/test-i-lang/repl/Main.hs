{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Main (
    main
  ) where

import Data.Monoid ((<>))
import Text.Trifecta.Rendering (Span(..))

import Repl (mkRepl)
import Language (mkLanguageDefaultParser)
import Component (ComponentInput)

import TestLanguage (languageRules, errorRules, TypeError, Type, Term(..))

main :: IO ()
main =
    mkRepl .
    mkLanguageDefaultParser $
  --  (languageRules <> errorRules)
   ((languageRules <> errorRules) :: ComponentInput () TypeError Type Term)
