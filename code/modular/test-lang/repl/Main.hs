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

import Repl (mkRepl)
import Language (mkLanguageDefaultParser)

import TestLanguage (languageRules, errorRulesSrcLoc)

main :: IO ()
main =
    mkRepl .
    mkLanguageDefaultParser $
    languageRules <> errorRulesSrcLoc
