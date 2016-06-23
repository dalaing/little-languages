{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Repl (
    mkRepl
  ) where

import           Control.Lens                 (view)
import           Control.Monad.IO.Class       (liftIO)
import           System.Console.Haskeline     (InputT, defaultSettings,
                                               getInputLine, runInputT)
import           Text.PrettyPrint.ANSI.Leijen (Doc, line, putDoc, text, (<+>),
                                               (<>))
import Text.Trifecta.Rendering (Span)
import Data.Constraint ((\\), (:-))
import Data.Constraint.Forall (ForallT, instT)

import           Common.Parse                 (parseFromString)
import           Component                    (ComponentOutput)
import           Component.Term.Infer         (HasInferOutput (..), runInfer)
import           Component.Term.Parse         (HasParseTermOutput (..))
import           Component.Term.Pretty        (HasPrettyTermOutput (..))
import           Component.Term.Eval.SmallStep (HasSmallStepOutput (..))
import           Component.Type.Pretty        (HasPrettyTypeOutput (..))
import           Component.Type.Error.Pretty  (HasPrettyTypeErrorOutput (..))
import Component.Term.Note (WithNoteTerm)
import Extras (Monoid2(..))

mkParseAndEval :: forall r e ty tm. (
                    WithNoteTerm tm
                  , Monoid2 r
                  )
               => ComponentOutput r e ty tm
               -> String
               -> Doc
mkParseAndEval c s =
  let
    parseTerm' = view parseTerm c
    infer' = view infer c
    prettyTerm' = view prettyTerm c
    prettyType' = view prettyType c
    prettyTypeError' = view prettyTypeErrorSrcLoc c
    smallStepEval' = view smallStepEval c
    ctx :: r Span String
    ctx = mempty \\ (spanMonoid2 :: Ord String :- Monoid (r Span String))
  in
    case parseFromString parseTerm' s of
      Left d -> d
      Right tm -> case runInfer ctx . infer' $ tm of
        Left e ->
          prettyTypeError' e
        Right ty ->
          prettyTerm' (smallStepEval' tm) <+>
          text ":" <+>
          prettyType' ty

mkRepl :: ( WithNoteTerm tm
          , Monoid2 r
          )
       => ComponentOutput r e ty tm
       -> IO ()
mkRepl c =
  runInputT defaultSettings loop
    where
      parseAndEval = mkParseAndEval c
      loop :: InputT IO ()
      loop = do
        i <- getInputLine "> "
        case i of
          Nothing -> return ()
          Just "quit" -> return ()
          Just i' -> do
            liftIO . putDoc . (<> line) . parseAndEval $ i'
            loop
