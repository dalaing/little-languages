{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Type.Error.UnknownType (
    AsUnknownType(..)
  , unknownTypeInput
  ) where

import           Control.Lens                        (preview)
import           Text.PrettyPrint.ANSI.Leijen        (Doc, text)

import           Component.Type.Error.UnknownType.Class (AsUnknownType (..))
import           Component                           (ComponentInput (..))
import           Component.Type.Error                (TypeErrorInput (..))
import           Component.Type.Error.Pretty         (PrettyTypeErrorInput (..),
                                                      PrettyTypeErrorRule (..))

prettyUnknownType :: AsUnknownType e
                  => e
                  -> Maybe Doc
prettyUnknownType =
    let
      prettyUnknownType' _ =
        text "Unknown type"
    in
      fmap prettyUnknownType' .
      preview _UnknownType

unknownTypeTypeErrorInput :: AsUnknownType e
                          => TypeErrorInput e ty n
unknownTypeTypeErrorInput =
  TypeErrorInput
   (PrettyTypeErrorInput [PrettyTypeErrorBase prettyUnknownType])

unknownTypeInput :: AsUnknownType e
                 => ComponentInput r e ty nTy tm nTm a
unknownTypeInput =
  ComponentInput
    mempty
    unknownTypeTypeErrorInput
    mempty
