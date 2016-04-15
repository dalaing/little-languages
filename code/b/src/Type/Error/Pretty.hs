{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Pretty printers for type errors in the B language.
-}
module Type.Error.Pretty (
    prettyTypeError
  ) where

import Text.PrettyPrint.ANSI.Leijen(Doc, text, (<+>), hang)
import qualified Text.PrettyPrint.ANSI.Leijen as PP ((<$>))

import Type.Pretty (prettyType)
import Type.Error (TypeError(..))

-- |
prettyTypeError :: TypeError -- ^
                -> Doc       -- ^
prettyTypeError (Unexpected ac ex) =
  hang 2 (text "Unexpected type:" PP.<$>
          text "actual:" <+> prettyType ac PP.<$>
          text "expected:" <+> prettyType ex)
prettyTypeError (ExpectedEq t1 t2) =
  hang 2 (text "Expected these types to be equal:" PP.<$>
          text "type 1:" <+> prettyType t1 PP.<$>
          text "type 2:" <+> prettyType t2)
prettyTypeError UnknownType =
  text "Unknown type"
