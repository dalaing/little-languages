{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Pretty printers for types of the B language.
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

-- | A pretty printer for 'TyBool'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTyBool) $ TyBool
-- Bool
prettyTyBool :: Type
             -> Maybe Doc
prettyTyBool TyBool =
  Just $ reservedConstructor "Bool"

-- | The set of pretty printing rules for types of the B language.
prettyTypeRules :: [Type -> Maybe Doc]
prettyTypeRules =
  [prettyTyBool]

-- | The pretty printer for types of the B language.
--
-- This function is built from the contents of 'prettyTypeRules'.
-- It will print "???" if none of the rules apply - which should never happen.
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
