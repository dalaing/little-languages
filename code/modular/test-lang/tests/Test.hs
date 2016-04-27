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

import Test.Tasty (defaultMain)
import Data.Monoid ((<>))

import Tests (mkTests)
import Component (ComponentInput)
import Language (mkLanguageDefaultParser)
import Component.Type.STLC (Context)

import TestLanguage (languageRules, errorRules, TypeError, Type, Term)

main :: IO ()
main =
  defaultMain .
  mkTests .
  mkLanguageDefaultParser $
  ((languageRules <> errorRules) :: ComponentInput (Context Type () String) (TypeError () String) Type () Term () String)

