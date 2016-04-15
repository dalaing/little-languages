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

import           Data.Foldable                (asum)
import           Data.Maybe                   (fromMaybe)

import           Common.Pretty                (reservedConstructor)
import           Text.PrettyPrint.ANSI.Leijen (Doc, text)

import           Type                         (Type (..))

-- |
prettyTyBool :: Type      -- ^
             -> Maybe Doc -- ^
prettyTyBool TyBool =
  Just $ reservedConstructor "Bool"

-- |
prettyTypeRules :: [Type -> Maybe Doc]
prettyTypeRules =
  [prettyTyBool]

-- |
prettyType :: Type -- ^
           -> Doc  -- ^
prettyType tm =
  fromMaybe (text "???") .
  asum .
  fmap ($ tm) $
  prettyTypeRules
