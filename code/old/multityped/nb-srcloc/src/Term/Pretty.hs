module Term.Pretty where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Term

printZero :: Term l
          -> Maybe Doc
printZero =
  fmap (const . text $ "O") .
  preview _TmZero

printSucc :: (Term l -> Doc)
          -> Term l
          -> Maybe Doc
printSucc pr =
    fmap printSucc' .
    preview _TmSucc
  where
    printSucc' n = text "S" <+> pr n

printPred :: (Term l -> Doc)
          -> Term l
          -> Maybe Doc
printPred pr =
    fmap printPred' .
    preview _TmPred
  where
    printPred' n = text "pred" <+> pr n

printTrue :: Term l
          -> Maybe Doc
printTrue =
  fmap (const . text $ "true") .
  preview _TmTrue

printFalse :: Term l
           -> Maybe Doc
printFalse =
  fmap (const . text $ "false") .
  preview _TmFalse

-- TODO some kind of indentation?
printIf :: (Term l -> Doc)
        -> Term l
        -> Maybe Doc
printIf pr =
    fmap printIf' .
    preview _TmIf
  where
   printIf' (b, c1, c2) =
     text "if" <+> pr b </>
     text "then" <+> pr c1 </>
     text "else" <+> pr c2

printIsZero :: (Term l -> Doc)
            -> Term l
            -> Maybe Doc
printIsZero pr =
    fmap printIsZero' .
    preview _TmIsZero
  where
   printIsZero' t =
     text "isZero" <+> pr t

printLoc :: (Term l -> Doc)
         -> Term l
         -> Maybe Doc
printLoc pr =
    fmap printLoc' .
    preview _TmLoc
  where
    printLoc' (_,t) = pr t

printTerm :: Term l
          -> Doc
printTerm = printTermDepth Nothing

printTermDepth :: Maybe Int -> Term l -> Doc
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
    , printTrue
    , printFalse
    , printIf child
    , printIsZero child
    , printLoc child
    ]

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyTermString :: Term l -> String
prettyTermString = docString . printTerm

