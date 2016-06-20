{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Pretty printers for type errors in the NB language.
-}
module Type.Error.Pretty (
    prettyTypeError
  ) where

-- from 'base'
import           Data.Foldable                (asum)
import           Data.Maybe                   (fromMaybe)

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc, hang, text, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP ((<$>))

-- local
import           Type.Error                   (TypeError (..))
import           Type.Pretty                  (prettyType)

-- $setup
-- >>> import Type
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let render r w f d = putStr $ displayS (renderPretty r w (plain (f d))) ""

-- |
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTeUnexpected) $ Unexpected TyNat TyBool
-- Unexpected type:
--   actual: Nat
--   expected: Bool
prettyTeUnexpected :: TypeError
                   -> Maybe Doc
prettyTeUnexpected (Unexpected ac ex) =
  Just . hang 2 $
    text "Unexpected type:" PP.<$>
    text "actual:" <+> prettyType ac PP.<$>
    text "expected:" <+> prettyType ex
prettyTeUnexpected _ =
  Nothing

-- |
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTeExpectedEq) $ ExpectedEq TyNat TyBool
-- Expected these types to be equal:
--   type 1: Nat
--   type 2: Bool
prettyTeExpectedEq :: TypeError
                   -> Maybe Doc
prettyTeExpectedEq (ExpectedEq t1 t2) =
  Just . hang 2 $
    text "Expected these types to be equal:" PP.<$>
    text "type 1:" <+> prettyType t1 PP.<$>
    text "type 2:" <+> prettyType t2
prettyTeExpectedEq _ =
  Nothing

-- |
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTeUnknownType) $ UnknownType
-- Unknown type
prettyTeUnknownType :: TypeError
                    -> Maybe Doc
prettyTeUnknownType UnknownType =
  Just $ text "Unknown type"
prettyTeUnknownType _ =
  Nothing

-- | Pretty type error rules
prettyTypeErrorRules :: [TypeError -> Maybe Doc]
prettyTypeErrorRules = [
    prettyTeUnexpected
  , prettyTeExpectedEq
  , prettyTeUnknownType
  ]

-- | Pretty prints a 'TypeError'.
--
-- >>> render 0.5 40 prettyTypeError $ Unexpected TyNat TyBool
-- Unexpected type:
--   actual: Nat
--   expected: Bool
--
-- >>> render 0.5 40 prettyTypeError $ ExpectedEq TyNat TyBool
-- Expected these types to be equal:
--   type 1: Nat
--   type 2: Bool
--
-- >>> render 0.5 40 prettyTypeError $ UnknownType
-- Unknown type
prettyTypeError :: TypeError
                -> Doc
prettyTypeError te =
  fromMaybe (text "???") .
  asum .
  fmap ($ te) $
  prettyTypeErrorRules
