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
import           Text.PrettyPrint.ANSI.Leijen (Doc, hang, text)
import qualified Text.PrettyPrint.ANSI.Leijen as PP ((<$>))

-- local
import           Common.Pretty                (tabulate)
import           Type.Error                   (TypeError (..))
import           Type.Pretty                  (prettyType)

-- $setup
-- >>> import Type
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let render r w f d = putStr $ displayS (renderPretty r w (plain (f d))) ""

-- | A pretty printer for 'Unexpected'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTeUnexpected) $ Unexpected TyNat TyBool
-- Unexpected type:
--   actual:   Nat
--   expected: Bool
prettyTeUnexpected :: TypeError
                   -> Maybe Doc
prettyTeUnexpected (Unexpected ac ex) =
  Just . hang 2 $
    text "Unexpected type:" PP.<$>
    tabulate [
        ("actual:", prettyType ac)
      , ("expected:", prettyType ex)
      ]
prettyTeUnexpected _ =
  Nothing

-- | A pretty printer for 'ExpectedEq'
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
    tabulate [
        ("type 1:", prettyType t1)
      , ("type 2:", prettyType t2)
      ]
prettyTeExpectedEq _ =
  Nothing

-- | A pretty printer for 'NoMatchingTypeRule'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTeNoMatchingTypeRule) $ NoMatchingTypeRule
-- No matching type rule
prettyTeNoMatchingTypeRule :: TypeError
                    -> Maybe Doc
prettyTeNoMatchingTypeRule NoMatchingTypeRule =
  Just $ text "No matching type rule"
prettyTeNoMatchingTypeRule _ =
  Nothing

-- | The set of pretty printing rules for type errors of the NB language.
prettyTypeErrorRules :: [TypeError -> Maybe Doc]
prettyTypeErrorRules = [
    prettyTeUnexpected
  , prettyTeExpectedEq
  , prettyTeNoMatchingTypeRule
  ]

-- | The pretty printer for type errors of the NB language.
--
-- This function is built from the contents of 'prettyTypeErrorRules'.
-- It will print "???" if none of the rules apply - which should never happen.
--
-- >>> render 0.5 40 prettyTypeError $ Unexpected TyNat TyBool
-- Unexpected type:
--   actual:   Nat
--   expected: Bool
--
-- >>> render 0.5 40 prettyTypeError $ ExpectedEq TyNat TyBool
-- Expected these types to be equal:
--   type 1: Nat
--   type 2: Bool
--
-- >>> render 0.5 40 prettyTypeError $ NoMatchingTypeRule
-- No matching type rule
prettyTypeError :: TypeError
                -> Doc
prettyTypeError te =
  fromMaybe (text "???") .
  asum .
  fmap ($ te) $
  prettyTypeErrorRules
