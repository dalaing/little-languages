module ExpressionPrinter where

import Control.Monad
import Data.Foldable

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

data Assoc = AssocLeft
           | AssocRight
           | AssocNone
           deriving (Eq, Ord, Show)

data PrintOperator t =
    Infix (t -> Maybe (t, t)) (Doc -> Doc -> Doc) Assoc
  | Prefix (t -> Maybe t) (Doc -> Doc)
  | Postfix (t -> Maybe t) (Doc -> Doc)

poApply :: PrintOperator t
        -> (t -> Maybe Doc)
        -> t
        -> Maybe Doc
poApply (Infix m p _) base t = do
  (x, y) <- m t
  x' <- base x
  y' <- base y
  return $ p x' y'
poApply (Prefix m p) base t = do
  x <- m t
  x' <- base x
  return $ p x'
poApply (Postfix m p) base t = do
  x <- m t
  x' <- base x
  return $ p x'

-- TODO come back here and deal with associativity and precedence properly
-- probably worth dealing with prefix and postfix ops at the same time,
-- for parity with the expression parser

-- generalize from Maybe to MonadPlus?
buildExpressionPrinter :: [[PrintOperator t]]
                       -> (t -> Maybe Doc)
                       -> t
                       -> Maybe Doc
buildExpressionPrinter os base =
  let
    parensBase = fmap parens . base
    ep t = asum . map (\o -> poApply o parensBase t) . concat $ os
  in
    ep

