{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Pretty printers for type errors in the I language.
-}
module Type.Error.Pretty (
    prettyTypeError
  ) where

-- from ansi-wl-pprint
import           Text.PrettyPrint.ANSI.Leijen (Doc, hang, text, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP ((<$>))

-- local
import           Type.Error                   (TypeError (..))
import           Type.Pretty                  (prettyType)

-- | Pretty prints a 'TypeError'.

-- No examples because we only have one type at the moment :/
prettyTypeError :: TypeError
                -> Doc
prettyTypeError (Unexpected ac ex) =
  hang 2 (text "Unexpected type:" PP.<$>
          text "actual:" <+> prettyType ac PP.<$>
          text "expected:" <+> prettyType ex)
prettyTypeError (ExpectedEq t1 t2) =
  hang 2 (text "Expected these types to be equal:" PP.<$>
          text "type 1:" <+> prettyType t1 PP.<$>
          text "type 2:" <+> prettyType t2)
prettyTypeError NoMatchingTypeRule =
  text "No matching type rule"
