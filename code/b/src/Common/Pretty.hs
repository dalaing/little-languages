{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Helpers for pretty printing.
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

-- from 'ansi-wl-pprint'
import           Text.Parser.Token.Highlight  (Highlight (..))
import           Text.PrettyPrint.ANSI.Leijen (Doc, displayS, plain,
                                               renderPretty, text)

-- from 'trifecta'
import           Text.Trifecta.Highlight      (withHighlight)

-- $setup
-- >>> import Text.PrettyPrint.ANSI.Leijen

-- | Converts a 'Doc' to a 'String' after stripping out any colours or other escape codes.
--
-- This is useful for debugging and testing.
--
-- >>> putStr $ prettyToString (text "test")
-- test
--
-- >>> putStr $ prettyToString (green (text "test"))
-- test
prettyToString :: Doc -> String
prettyToString d =
  displayS (renderPretty 0.75 80 (plain d)) ""

-- | Highlights a string using the same markup as 'trifecta' for things
-- tagged as 'ReservedIdentifier's by a token parser.
reservedIdentifier :: String
                   -> Doc
reservedIdentifier =
  withHighlight ReservedIdentifier .
  text

-- | Highlights a string using the same markup as 'trifecta' for things
-- tagged as 'Identifier's by a token parser.
identifier :: String
           -> Doc
identifier =
  withHighlight Identifier .
  text

-- | Highlights a string using the same markup as 'trifecta' for things
-- tagged as 'ReservedConstructor's by a token parser.
reservedConstructor :: String
                    -> Doc
reservedConstructor =
  withHighlight ReservedConstructor .
  text

-- | Highlights a string using the same markup as 'trifecta' for things
-- tagged as 'Constructor's by a token parser.
constructor :: String
            -> Doc
constructor =
  withHighlight Constructor .
  text

-- | Highlights a string using the same markup as 'trifecta' for things
-- tagged as 'ReservedOperator's by a token parser.
reservedOperator :: String
                 -> Doc
reservedOperator =
  withHighlight ReservedOperator .
  text

-- | Highlights a string using the same markup as 'trifecta' for things
-- tagged as 'Operator's by a token parser.
operator :: String
         -> Doc
operator =
  withHighlight Operator .
  text
