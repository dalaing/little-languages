{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Type.STLC.Pretty (
    prettyTypeInput
  ) where

import           Control.Lens                 (preview)
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>))

import           Common.Text (Assoc(..), ExpressionInfo(..))
import           Common.Pretty                (reservedOperator)
import           Component.Type.Pretty        (PrettyTypeInput(..), PrettyTypeRule (..))

import           Component.Type.STLC         (AsSTLCType (..), WithSTLCType)

prettyTyArr :: Doc
            -> Doc
            -> Doc
prettyTyArr tm1 tm2 =
  tm1 <+>
  reservedOperator "->" <+>
  tm2

-- |
prettyTypeInput :: WithSTLCType ty
                => PrettyTypeInput ty
prettyTypeInput =
  PrettyTypeInput
    [ PrettyTypeExpression
      (ExpressionInfo AssocRight 6)
      (preview _TyArr)
      prettyTyArr
    ]
