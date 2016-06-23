module UI where

import Text.PrettyPrint.ANSI.Leijen (Doc)

import Term.Parse
import Term.Pretty
import Term.Eval.SmallStep

parseAndEval :: String -> Doc
parseAndEval s =
  case parseTermString s of
    Left e -> e
    Right t -> prettyTerm . sEval $ t
