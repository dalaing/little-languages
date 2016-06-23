module Term.Pretty where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Term

printTrue :: Term -> Maybe Doc
printTrue = fmap (const . text $ "true") . preview _TmTrue

printFalse :: Term -> Maybe Doc
printFalse = fmap (const . text $ "false") . preview _TmFalse

-- TODO some kind of indentation?
printIf :: (Term -> Doc) -> Term -> Maybe Doc
printIf pr = fmap printIf' . preview _TmIf
  where
   printIf' (b, c1, c2) =
     text "if" <+> pr b </>
     text "then" <+> pr c1 </>
     text "else" <+> pr c2

printTerm :: Term -> Doc
printTerm = printTermDepth Nothing

printTermDepth :: Maybe Int -> Term -> Doc
printTermDepth (Just 0) _ = text "..."
printTermDepth d t =
  fromMaybe empty .
  asum .
  map ($ t) $ [
    printTrue
  , printFalse
  , printIf (printTermDepth (fmap (subtract 1) d))
  ]

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyString :: Term -> String
prettyString = docString . printTerm

