module Type.Pretty where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.PrettyPrint.ANSI.Leijen

import Type

printTyBool :: Type -> Maybe Doc
printTyBool = fmap (const . text $ "Bool") . preview _TyBool

printTyNat :: Type -> Maybe Doc
printTyNat = fmap (const . text $ "Nat") . preview _TyNat

prettyType :: Type -> Doc
prettyType t =
  fromMaybe empty .
  asum .
  map ($ t) $ [
    printTyBool
  , printTyNat
  ]

docString :: Doc -> String
docString d = displayS (renderPretty 0.4 40 (plain d)) ""

prettyString :: Type -> String
prettyString = docString . prettyType
