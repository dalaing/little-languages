module Term.Pretty where

import Control.Applicative ((<|>))
import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Parser.Expression (Assoc(..))

import Term

printInt :: Term -> Maybe Doc
printInt = fmap (integer . toInteger) . preview _TmInt

data PrintOperator t =
  Infix (t -> Maybe (t, t)) (Doc -> Doc -> Doc) Assoc

poApply :: PrintOperator t -> (t -> Maybe Doc) -> t -> Maybe Doc
poApply (Infix m p _) base t = do
  (x, y) <- m t
  x' <- base x
  y' <- base y
  return $ p x' y'

printAdd :: Doc -> Doc -> Doc
printAdd x y = x <+> text "+" <+> y

printSub :: Doc -> Doc -> Doc
printSub x y = x <+> text "-" <+> y

printMul :: Doc -> Doc -> Doc
printMul x y = x <+> text "*" <+> y

printExp :: Doc -> Doc -> Doc
printExp x y = x <+> text "^" <+> y

-- TODO come back here and deal with associativity and precedence properly
-- probably worth dealing with prefix and postfix ops at the same time,
-- for parity with the expression parser
buildExpressionPrinter :: [[PrintOperator Term]]
                       -> (Term -> Maybe Doc)
                       -> Term
                       -> Maybe Doc
buildExpressionPrinter os base =
  let
    ep t = asum . map (\o -> poApply o base t) . concat $ os
  in
    ep

printExpr :: Term -> Maybe Doc
printExpr = buildExpressionPrinter table printTerm
  where
    table =
      [
        [ Infix (preview _TmAdd) printAdd AssocLeft
        , Infix (preview _TmSub) printSub AssocLeft
        ]
      , [Infix (preview _TmMul) printMul AssocLeft]
      , [Infix (preview _TmExp) printExp AssocRight]
      ]

-- TODO return to optionally depth-limited printing later on
printTerm :: Term -> Maybe Doc
printTerm t = printInt t <|> fmap parens (printExpr t)

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyString :: Term -> String
prettyString = docString . fromMaybe empty . printTerm

prettyTerm :: Term -> Doc
prettyTerm = fromMaybe empty . printTerm

