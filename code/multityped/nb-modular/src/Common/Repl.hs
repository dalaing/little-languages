module Common.Repl where

import Control.Monad.IO.Class
import Control.Monad.Except (runExcept)
import Control.Lens (view)
import System.Console.Haskeline
import Text.PrettyPrint.ANSI.Leijen (Doc, putDoc, (<>), (<+>), text, line)

import Common.Term
import Common.Term.Infer
import Common.Term.Eval.SmallStep
import Common.Term.Parse
import Common.Term.Pretty
import Common.Type
import Common.Type.Pretty
import Common.Type.Error.Pretty

mkRepl :: TypeOutput e ty
       -> TermOutput e ty tm
       -> IO ()
mkRepl ty tm =
    runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      i <- getInputLine "> "
      case i of
        Nothing -> return ()
        Just "quit" -> return ()
        Just i' -> do
          liftIO . putDoc . (<> line) . parseAndEval ty tm $ i'
          loop

parseAndEval :: TypeOutput e ty
             -> TermOutput e ty tm
             -> String
             -> Doc
parseAndEval tyO tmO s =
  case (view parseTermString tmO) s of
    Left e -> e
    Right tm -> case (runExcept . runInfer . view infer tmO) tm of
      Left e -> (view prettyTypeError tyO) e
      Right ty -> (view prettyTerm tmO) ((view smallStepEval tmO) tm) <+> text ":" <+> (view prettyType tyO) ty
