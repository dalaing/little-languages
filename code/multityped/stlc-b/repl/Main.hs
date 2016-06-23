module Main where

import Control.Monad.IO.Class
import System.Console.Haskeline

import Text.PrettyPrint.ANSI.Leijen (putDoc, (<>), line)

import UI

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
