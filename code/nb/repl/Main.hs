{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Main (
    main
  ) where

-- from 'transformers'
import           Control.Monad.IO.Class       (liftIO)

-- from 'haskeline'
import           System.Console.Haskeline     (InputT, defaultSettings,
                                               getInputLine, runInputT)

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc, line, putDoc, text, (<+>),
                                               (<>))

-- local
import           Common.Parse                 (parseFromString)
import           Term.Eval.SmallStep          (eval)
import           Term.Infer                   (inferTerm, runInfer)
import           Term.Parse                   (parseTerm)
import           Term.Pretty                  (prettyTerm)
import           Type.Error.Pretty            (prettyTypeError)
import           Type.Pretty                  (prettyType)

-- | Parses a string into a term, typechecks the term, and then evaluates it.
parseAndEval :: String
             -> Doc
parseAndEval s =
  case parseFromString parseTerm s of
    Left d -> d
    Right tm -> case runInfer . inferTerm $ tm of
      Left e -> prettyTypeError e
      Right ty ->
        prettyTerm (eval tm) <+> text ":" <+> prettyType ty

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      i <- getInputLine "> "
      case i of
        Nothing -> return ()
        Just "quit" -> return ()
        Just i' -> do
          liftIO . putDoc . (<> line) . parseAndEval $ i'
          loop
