module Term.Pretty where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Term

printTrue :: Term n l a
          -> Maybe Doc
printTrue =
  fmap (const . text $ "true") .
  preview _TmTrue

printFalse :: Term n l a
           -> Maybe Doc
printFalse =
  fmap (const . text $ "false") .
  preview _TmFalse

-- TODO some kind of indentation?
printIf :: (Term n l a -> Doc)
        -> Term n l a
        -> Maybe Doc
printIf pr =
    fmap printIf' .
    preview _TmIf
  where
   printIf' (b, c1, c2) =
     text "if" <+> pr b </>
     text "then" <+> pr c1 </>
     text "else" <+> pr c2

printLoc :: (Term n l a -> Doc)
         -> Term n l a
         -> Maybe Doc
printLoc pr =
    fmap printLoc' .
    preview _TmLoc
  where
    printLoc' (_,t) = pr t

printTerm :: Term n l a
          -> Doc
printTerm =
  printTermDepth Nothing

printTermDepth :: Maybe Int
               -> Term n l a
               -> Doc
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
    , printLoc child
    ]

docString :: Doc
          -> String
docString d =
  displayS (renderPretty 0.4 40 (plain d)) ""

prettyTermString :: Term n l a
                 -> String
prettyTermString =
  docString . printTerm

