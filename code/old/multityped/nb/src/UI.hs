module UI where

import Control.Monad.Except
import Text.PrettyPrint.ANSI.Leijen (Doc, (<+>), text)

import Term.Parse
import Term.Pretty
import Term.Eval.SmallStep

import Type.Pretty
import Type.Error.Pretty
import Type.Infer

parseAndEval :: String -> Doc
parseAndEval s =
  case parseTermString s of
    Left e -> e
    Right tm -> case (runExcept . infer) tm of
      Left e -> prettyError e
      Right ty -> printTerm (sEval tm) <+> text ":" <+> prettyType ty
