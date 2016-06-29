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
  , tabulate
  ) where

-- from 'ansi-wl-pprint'
import           Text.Parser.Token.Highlight  (Highlight (..))
import           Text.PrettyPrint.ANSI.Leijen (Doc, displayS, fill, plain,
                                               renderPretty, text, vcat, (<+>))

-- from 'trifecta'
import           Text.Trifecta.Highlight      (withHighlight)

-- $setup
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let render r w f d = putStr $ displayS (renderPretty r w (plain (f d))) ""

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

-- | Combines a list of labels and `Doc`s into a single `Doc`.
--
-- The labels are padded on the right so that they all have the same length.
--
-- >>> render 0.5 40 tabulate [("label1:", text "doc 1"), ("label12:", text "doc 2"), ("label123:", text "doc 3")]
-- label1:   doc 1
-- label12:  doc 2
-- label123: doc 3
tabulate :: [(String, Doc)] -> Doc
tabulate xs =
    vcat .
    fmap pad $
    xs
  where
    pad (label, doc) =
      fill maxLength (text label) <+> doc
    maxLength =
      maximum .
      fmap (length . fst) $
      xs
