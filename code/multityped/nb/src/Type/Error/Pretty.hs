module Type.Error.Pretty where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Type.Pretty
import Type.Error

prettyError :: TypeError
            -> Doc
prettyError (TeUnexpected ac ex) =
  hang 2 (text "Unexpected type:" PP.<$>
          text "actual:" <+> prettyType ac PP.<$>
          text "expected:" <+> prettyType ex)
prettyError (TeExpectedEq t1 t2) =
  hang 2 (text "Expected these types to be equal:" PP.<$>
          text "type 1:" <+> prettyType t1 PP.<$>
          text "type 2:" <+> prettyType t2)
prettyError TeUnknownType =
  text "Unknown type"
