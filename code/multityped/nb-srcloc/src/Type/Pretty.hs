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

printTyNat :: Type l
           -> Maybe Doc
printTyNat =
  fmap (const . text $ "Nat") .
  preview _TyNat

printTyLoc :: (Type l -> Doc)
           -> Type l
           -> Maybe Doc
printTyLoc pr =
  fmap (pr . snd) .
  preview _TyLoc

prettyType :: Type l
           -> Doc
prettyType t =
  fromMaybe empty .
  asum .
  map ($ t) $ [
    printTyBool
  , printTyNat
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
