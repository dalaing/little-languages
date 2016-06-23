module Term.Pretty where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Term

printZero :: Term -> Maybe Doc
printZero = fmap (const . text $ "O") . preview _TmZero

printSucc :: (Term -> Doc) -> Term -> Maybe Doc
printSucc pr = fmap printSucc' . preview _TmSucc
  where
    printSucc' n = text "S" <+> pr n

printPred :: (Term -> Doc) -> Term -> Maybe Doc
printPred pr = fmap printPred' . preview _TmPred
  where
    printPred' n = text "pred" <+> pr n

printTerm :: Term -> Doc
printTerm = printTermDepth Nothing

printTermDepth :: Maybe Int -> Term -> Doc
printTermDepth (Just 0) _ = text "..."
printTermDepth d t =
  let
    child = printTermDepth (fmap (subtract 1) d)
  in
    fromMaybe empty .
    asum .
    map ($ t) $ [
      printZero
    , printSucc child
    , printPred child
    ]

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyString :: Term -> String
prettyString = docString . printTerm

