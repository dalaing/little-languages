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
import           Data.Maybe (fromMaybe)

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc, displayS, plain,
                                               renderPretty, text, parens)

-- from 'parsers'
import           Text.Parser.Expression       (Assoc(..))
import           Text.Parser.Token.Highlight  (Highlight (..))

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

-- | Rules for pretty printing languages.
data PrettyRule a =
    PrettyRegular (a -> Maybe Doc)                               -- ^ A pretty printer for 'a'.
  | PrettyOp OperatorInfo (a -> Maybe (a, a)) (Doc -> Doc -> Doc) -- ^ A rule for pretty printing for an infix binary operator on 'a', made up of information about the operator, a function for matching on the operator and returning the arguments, and a function for printing the operator given the pretty printed form of the arguments to the operator.

-- | Gathers the regular pretty printing rules.
gatherRegular :: a
              -> PrettyRule a
              -> Maybe Doc
gatherRegular t (PrettyRegular f) =
  f t
gatherRegular _ _ =
  Nothing

-- | Finds the 'OperatorInfo' for 'a', if any exists.
findOperatorInfo :: [PrettyRule a]
                 -> a
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

-- | Describes an argument to a binary operator.
data Argument =
    ArgumentLeft  -- ^ The argument on the left.
  | ArgumentRight -- ^ The argument on the right.
  deriving (Eq, Ord, Show)

-- | Determines if an argument to an operator is in a position where it might avoid need brackets, given the associativity of the operator.
argumentAssociates :: Argument
                   -> Assoc
                   -> Bool
argumentAssociates ArgumentLeft AssocLeft =
  True
argumentAssociates ArgumentRight AssocRight =
  True
argumentAssociates _ _ =
  False

-- | Works out if the argument to an operator needs parentheses.
needsParens :: Argument           -- ^ The position of the argument.
            -> OperatorInfo       -- ^ The operator info for the current operator.
            -> Maybe OperatorInfo -- ^ The operator info for the argument to the current operator, if any (typically the output of 'findOperatorInfo')a.
            -> Bool               -- ^ Whether we should put parentheses around the argument.
needsParens _ _ Nothing =
  -- if the argument is not an operator, then we don't need parentheses
  False
needsParens arg info (Just argInfo) =
  -- we need parens if the operator has  no associativity
  assoc info == AssocNone ||
  -- or if the argument has a lower precedence than the current operator
  precedence argInfo < precedence info ||
  -- or if the argument has the same precedence as the current operator but the argument
  -- is not in the position matching the associativity of this operator
  (precedence argInfo == precedence info && not (argumentAssociates arg (assoc info)))

-- | Gathers the operator pretty printing rules.
gatherOp :: a                         -- ^ The thing we're printing.
         -> (a -> Doc)                -- ^ The combined printing rule (the result of 'mkPretty').
         -> (a -> Maybe OperatorInfo) -- ^ A function to search for operator info in the rules (the result of 'findOperatorInfo').
         -> PrettyRule a              -- ^ The rule to process.
         -> Maybe Doc                 -- ^ The pretty printed output, if any.
gatherOp t pretty findInfo (PrettyOp i match printer) = do
  (t1, t2) <- match t
  let addParens b =
        if b then parens else id
  let p1 =
        addParens .
        needsParens ArgumentLeft i .
        findInfo $
        t1
  let p2 =
        addParens .
        needsParens ArgumentRight i .
        findInfo $
        t2
  return $ printer (p1 . pretty $ t1) (p2 . pretty $ t2)
gatherOp _ _ _ _ =
  Nothing

-- | Combine a list of pretty printing rules into a pretty printing function.
mkPretty :: [PrettyRule a]
         -> a
         -> Doc
mkPretty rules =
  let
    prettyRegular tm =
      asum .
      fmap (gatherRegular tm) $
      rules
    findInfo =
      findOperatorInfo rules
    prettyOp tm =
      asum .
      fmap (gatherOp tm prettyTerm findInfo) $
      rules
    prettyTerm tm =
      fromMaybe (text "???") .
      asum .
      fmap ($ tm) $
      [ prettyRegular , prettyOp ]
  in
    prettyTerm
