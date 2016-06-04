{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Pretty printers for types of the NB language.
-}
module Type.Pretty (
    prettyTypeRules
  , prettyType
  ) where

-- from 'base'
import           Data.Foldable                (asum)
import           Data.Maybe                   (fromMaybe)

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc, text)

-- local
import           Common.Pretty                (reservedConstructor)
import           Type                         (Type (..))

-- $setup
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let render r w f d = putStr $ displayS (renderPretty r w (plain (f d))) ""

-- | A pretty printer for 'TyNat'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTyNat) $ TyNat
-- Nat
prettyTyNat :: Type
            -> Maybe Doc
prettyTyNat TyNat =
  Just $ reservedConstructor "Nat"
prettyTyNat _ =
  Nothing

-- | A pretty printer for 'TyBool'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTyBool) $ TyBool
-- Bool
prettyTyBool :: Type
             -> Maybe Doc
prettyTyBool TyBool =
  Just $ reservedConstructor "Bool"
prettyTyBool _ =
  Nothing

-- | The set of pretty printing rules for types of the NB language.
prettyTypeRules :: [Type -> Maybe Doc]
prettyTypeRules =
  [ prettyTyNat
  , prettyTyBool
  ]

-- | The pretty printer for types of the NB language.
--
-- This function is built from the contents of 'prettyTypeRules'.
-- It will print "???" if none of the rules apply - which should never happen.
--
-- >>> render 0.5 40 prettyType $ TyNat
-- Nat
--
-- >>> render 0.5 40 prettyType $ TyBool
-- Bool
prettyType :: Type
           -> Doc
prettyType tm =
  fromMaybe (text "???") .
  asum .
  fmap ($ tm) $
  prettyTypeRules
