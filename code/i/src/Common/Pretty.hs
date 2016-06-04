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
  , PrettyRule(..)
  , mkPretty
  ) where

-- from 'base'
import           Data.Foldable (asum)
import           Data.Maybe (fromMaybe, isNothing)

-- from 'ansi-wl-pprint'
import           Text.Parser.Token.Highlight  (Highlight (..))
import           Text.PrettyPrint.ANSI.Leijen (Doc, displayS, plain,
                                               renderPretty, text, parens)

-- from 'parsers'
import           Text.Parser.Expression       (Assoc(..))

-- from 'trifecta'
import           Text.Trifecta.Highlight      (withHighlight)

-- local
import           Common.Text                  (OperatorInfo(..))

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

-- |
data PrettyRule t =
    PrettyRegular (t -> Maybe Doc)                               -- ^
  | PrettyOp OperatorInfo (t -> Maybe (t, t)) (Doc -> Doc -> Doc) -- ^

tryRegular :: t
           -> PrettyRule t
           -> Maybe Doc
tryRegular t (PrettyRegular f) =
  f t
tryRegular _ _ =
  Nothing

findOperatorInfo :: [PrettyRule t]
                 -> t
                 -> Maybe OperatorInfo
findOperatorInfo rules tm =
    asum .
    fmap (checkOperatorInfo tm) $
    rules
  where
    checkOperatorInfo t (PrettyOp info match _) =
      info <$ match t
    checkOperatorInfo _ _ =
      Nothing

tryOp :: t
      -> (t -> Doc)
      -> (t -> Maybe OperatorInfo)
      -> PrettyRule t
      -> Maybe Doc
tryOp t pretty findInfo (PrettyOp i match printer) = do
  (t1, t2) <- match t
  let i1 = findInfo t1
  let i2 = findInfo t2
  let h1 = maybe True (\j -> _precedence j > _precedence i) i1
  let h2 = maybe True (\j -> _precedence j > _precedence i) i2
  let e1 = maybe True (\j -> _precedence j == _precedence i) i1
  let e2 = maybe True (\j -> _precedence j == _precedence i) i2
  let p1 =
        if (_assoc i == AssocLeft && e1) || h1
        then id
        else parens
  let p2 =
        if (_assoc i == AssocRight && e2) || h2
        then id
        else parens
  return $ printer (p1 . pretty $ t1) (p2 . pretty $ t2)
tryOp _ _ _ _ =
  Nothing

-- |
mkPretty :: [PrettyRule t]
         -> t
         -> Doc
mkPretty rules =
  let
    prettyTerm tm =
      asum .
      fmap (tryRegular tm) $
      rules
    findInfo =
      findOperatorInfo rules
    prettyOp tm =
      asum .
      fmap (tryOp tm prettyExpr findInfo) $
      rules
    prettyExpr tm =
      fromMaybe (text "???") .
      asum . fmap ($ tm) $
      [ prettyTerm , prettyOp ]
  in
    prettyExpr

{-
  fromMaybe (text "???") .
  asum .
  fmap ($ tm) $
  prettyTermRules
-}
