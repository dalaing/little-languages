module Term.Pretty where

import Control.Applicative ((<|>))
import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Parser.Expression (Assoc(..))

import Term

-- |
-- >>> prettyString text (TmVar "x")
-- "x"
printVar :: (n -> Doc) -> Term n n -> Maybe Doc
printVar pr = fmap pr . preview _TmVar

data PrintOperator t =
  Infix (t -> Maybe (t, t)) (Doc -> Doc -> Doc) Assoc

poApply :: PrintOperator t -> (t -> Maybe Doc) -> t -> Maybe Doc
poApply (Infix m p _) base t = do
  (x, y) <- m t
  x' <- base x
  y' <- base y
  return $ p x' y'

-- |
-- >>> prettyString text (TmApp (TmVar "x") (TmVar "y"))
-- "(x) @ (y)"
printApp :: Doc -> Doc -> Doc
printApp x y = x <+> text "@" <+> y

-- |
-- >>> prettyString text (lam "x" (TmApp (TmVar "x") (TmVar "y")))
-- "\ x -> (x) (y)"
printLam :: Eq n => (n -> Doc) -> (Term n n -> Maybe Doc) -> Term n n -> Maybe Doc
printLam pv pt = fmap printLam' . preview _lam
  where
    printLam' (v,e) =
      text "\\" <+>
      pv v <+>
      text "->" <+>
      (fromMaybe empty . pt $ e)


-- TODO come back here and deal with associativity and precedence properly
-- probably worth dealing with prefix and postfix ops at the same time,
-- for parity with the expression parser
buildExpressionPrinter :: [[PrintOperator (Term n n)]]
                       -> (Term n n -> Maybe Doc)
                       -> Term n n
                       -> Maybe Doc
buildExpressionPrinter os base =
  let
    baseparens = fmap parens . base
    ep t = asum . map (\o -> poApply o baseparens t) . concat $ os
  in
    ep

-- TODO tag App as high or low precedence so we know how to print it
-- maybe just start with the high precedence version?
printExpr :: Eq n => (n -> Doc) -> Term n n -> Maybe Doc
printExpr pr = buildExpressionPrinter table (printTerm pr)
  where
    table = [[Infix (preview _TmApp) printApp AssocRight]]

-- TODO return to optionally depth-limited printing later on
printTerm :: Eq n => (n -> Doc) -> Term n n -> Maybe Doc
printTerm pr t = fmap parens (printExpr pr t) <|>
                 printVar pr t <|>
                 printLam pr (printTerm pr) t

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyString :: Term String String -> String
prettyString = docString . fromMaybe empty . printTerm text

prettyTerm :: Term String String -> Doc
prettyTerm = fromMaybe empty . printTerm text

