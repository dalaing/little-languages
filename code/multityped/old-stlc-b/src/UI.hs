{-# LANGUAGE FlexibleContexts #-}
module UI where

import Control.Monad.Except

import Text.PrettyPrint.ANSI.Leijen

import Type
import Type.Pretty
import Type.Infer
import Type.Error
import Type.Error.Message

import Term
import Term.Parse
import Term.Pretty
import Term.Eval.SmallStep

parseAndTypecheckString :: MonadError Doc m => String -> m (Type, Term ())
parseAndTypecheckString s = do
    tm <- parseTermString s
    case runExcept (infer tm) of
      Left e -> throwError . toDoc $ e
      Right (TypeInfo ty _) -> return (ty, void . stripLoc . locToTerm $ tm)

parseTypecheckRunString :: MonadError Doc m => String -> m (Type, Term ())
parseTypecheckRunString = fmap (fmap sEval) . parseAndTypecheckString

parseTypecheckRun :: String -> IO ()
parseTypecheckRun s =
  let
    d = case runExcept (parseTypecheckRunString s) of
      Left e -> e
      Right (ty, tm) -> printTerm tm <+> text ":" <+> printType ty
  in
    putStrLn . docString $ d

