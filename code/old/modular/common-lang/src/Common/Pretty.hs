{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Common.Pretty (
    prettyToString
  , reservedIdentifier
  , identifier
  , reservedConstructor
  , constructor
  , reservedOperator
  , operator
  ) where

import           Text.Parser.Token.Highlight  (Highlight (..))
import           Text.PrettyPrint.ANSI.Leijen (Doc, displayS, plain,
                                               renderPretty, text)
import           Text.Trifecta.Highlight      (withHighlight)

-- |
prettyToString :: Doc    -- ^
               -> String -- ^
prettyToString d =
  displayS (renderPretty 0.4 80 (plain d)) ""

-- |
reservedIdentifier :: String -- ^
                   -> Doc    -- ^
reservedIdentifier =
  withHighlight ReservedIdentifier .
  text

-- |
identifier :: String -- ^
           -> Doc    -- ^
identifier =
  withHighlight Identifier .
  text

-- |
reservedConstructor :: String -- ^
                    -> Doc    -- ^
reservedConstructor =
  withHighlight ReservedConstructor .
  text

-- |
constructor :: String -- ^
            -> Doc    -- ^
constructor =
  withHighlight Constructor .
  text

-- |
reservedOperator :: String -- ^
                  -> Doc    -- ^
reservedOperator =
  withHighlight ReservedOperator .
  text

-- |
operator :: String -- ^
         -> Doc    -- ^
operator =
  withHighlight Operator .
  text
