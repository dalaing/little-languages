module Type.Pretty where

import Control.Lens (preview)
import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.Parser.Expression (Assoc(..))
import Text.PrettyPrint.ANSI.Leijen

import Type

data PrintOperator t =
  Infix (t -> Maybe (t, t)) (Doc -> Doc -> Doc) Assoc

poApply :: PrintOperator t -> (t -> Maybe Doc) -> t -> Maybe Doc
poApply (Infix m p _) base t = do
  (x, y) <- m t
  x' <- base x
  y' <- base y
  return $ p x' y'

-- TODO come back here and deal with associativity and precedence properly
-- probably worth dealing with prefix and postfix ops at the same time,
-- for parity with the expression parser
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

printTyBool :: Type l -> Maybe Doc
printTyBool = fmap (const . text $ "Bool") . preview _TyBool

printTyArr :: Doc -> Doc -> Doc
printTyArr x y = x <+> text "->" <+> y

printExpr :: Type l -> Maybe Doc
printExpr = buildExpressionPrinter table printType
  where
    table = [[Infix (preview _TyArr) printTyArr AssocRight]]

printType :: Type l -> Maybe Doc
printType t =
  fmap parens (printExpr t) <|>
  printTyBool t

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyString :: Type l -> String
prettyString = docString . fromMaybe empty . printExpr

