module Term.Pretty (
    prettyTerm
  ) where

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen
import Bound

import Term.AST

data BlockStyle = Indent | BracesAndColons

data TermPrintStyle = TermPrintStyle {
    apsVar :: Doc -> Doc 
  , apsOp :: Doc -> Doc
  , apsEnclose :: Doc -> Doc
  , apsLetBlock :: BlockStyle
  }

-- TODO change to colours
defaultTermPrintStyle :: TermPrintStyle
defaultTermPrintStyle = 
  TermPrintStyle 
    green
    blue
    white
    Indent

-- TODO change to Reader for the style
prettyTermWithStyle :: TermPrintStyle -> Term String -> Doc
prettyTermWithStyle s term = case term of
    Var x -> var x
    Lam e -> let
        v = freshVarForLam e
        t = instantiate1 (Var v) e
      in
        group . hang 2 $ (op "\\" <> var v <+> op "->" <+ prettyTermWithStyle s t)
    App x y -> let
        wrap1 = case x of
          l@(Lam _) -> par
          _ -> id
        wrap2 = case y of
          a@(App _ _) -> par
          _           -> id
      in
        wrap1 (prettyTermWithStyle s x) <+> op ".@." <+> wrap2 (prettyTermWithStyle s y)
  where
    var = apsVar s . string
    op = apsOp s . string
    par x = group . align $ (apsEnclose s lparen <+> x <+> apsEnclose s rparen)

prettyTerm :: Term String -> Doc
prettyTerm = prettyTermWithStyle defaultTermPrintStyle

