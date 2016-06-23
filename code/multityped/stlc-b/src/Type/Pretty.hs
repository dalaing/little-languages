module Type.Pretty where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.ANSI.Leijen

import Type

printTyBool :: Type l
            -> Maybe Doc
printTyBool =
  fmap (const . text $ "Bool") .
  preview _TyBool

-- TODO use expression printer
printTyArr :: (Type l -> Doc)
           -> Type l
           -> Maybe Doc
printTyArr pr =
    fmap printTyArr' .
    preview _TyArr
  where
    printTyArr' (t1, t2) =
      pr t1 <+> text "->" <+> pr t2

printTyLoc :: (Type l -> Doc)
           -> Type l
           -> Maybe Doc
printTyLoc pr =
  fmap (pr . snd) .
  preview _TyLoc

prettyType :: Type l
           -> Doc
prettyType t =
  fromMaybe (text "???") .
  asum .
  map ($ t) $ [
    printTyBool
  , printTyArr prettyType
  , printTyLoc prettyType
  ]

docString :: Doc
          -> String
docString d =
  displayS (renderPretty 0.4 40 (plain d)) ""

prettyTypeString :: Type l
                 -> String
prettyTypeString =
  docString .
  prettyType
