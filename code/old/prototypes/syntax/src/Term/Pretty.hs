module Term.Pretty (
    prettyTerm
  ) where

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen

import Bound
import Bound.Name
import Bound.Scope

import Term

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
prettyTermWithStyle :: TermPrintStyle -> Term l String String -> Doc
prettyTermWithStyle s term = case term of
    Var x -> var x
    Lam e -> let
        v = name . head . bindings $ e
        t = instantiate1Name (Var v) e
      in
        par $ op "lam" <+> var v <+> prettyTermWithStyle s t
    App x y -> par $ 
      prettyTermWithStyle s x <+> prettyTermWithStyle s y 
  where
    var = apsVar s . string
    op = apsOp s . string
    par x = group . align $ (apsEnclose s lparen <+> x <+> apsEnclose s rparen)

prettyTerm :: Term l String String -> Doc
prettyTerm = prettyTermWithStyle defaultTermPrintStyle


