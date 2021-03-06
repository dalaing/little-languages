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
import Data.Proxy (Proxy(..))

import Test.Tasty (defaultMain)
import Text.Trifecta.Rendering (Span)

import Tests (mkTests)
import Component (ComponentInput)
import Language (mkLanguageDefaultParser)

import TestLanguage (languageRules, errorRules, TypeError, Type, Term)

main :: IO ()
main =
  defaultMain .
  mkTests (Proxy :: Proxy Span) (Proxy :: Proxy Span) .
  mkLanguageDefaultParser $
  ((languageRules <> errorRules) :: ComponentInput () TypeError Type Term) 

