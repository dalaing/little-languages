module Term.Pretty where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Term

printTrue :: Term l n a -> Maybe Doc
printTrue = fmap (const . text $ "true") . preview _TmTrue

printFalse :: Term l n a -> Maybe Doc
printFalse = fmap (const . text $ "false") . preview _TmFalse

-- TODO some kind of indentation?
printIf :: (Term l n a -> Doc) -> Term l n a -> Maybe Doc
printIf pr = fmap printIf' . preview _TmIf
  where
   printIf' (b, c1, c2) =
     text "if" <+> pr b </>
     text "then" <+> pr c1 </>
     text "else" <+> pr c2

printTerm :: Term l n a -> Doc
printTerm = printTermDepth Nothing

printTermDepth :: Maybe Int -> Term l n a -> Doc
printTermDepth (Just 0) _ = text "..."
printTermDepth d t =
  let
    child = printTermDepth (fmap (subtract 1) d)
  in
    fromMaybe empty .
    asum .
    map ($ t) $ [
      printTrue
    , printFalse
    , printIf child
    ]

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyTermString :: Term l n a -> String
prettyTermString = docString . printTerm

