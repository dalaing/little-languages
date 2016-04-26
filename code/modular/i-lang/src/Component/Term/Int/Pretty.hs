{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Int.Pretty (
    prettyTermInput
  ) where

import           Control.Lens                 (preview)
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>), int)

import           Common.Text (Assoc(..), ExpressionInfo(..))
import           Common.Pretty                (reservedOperator)
import           Component.Term.Pretty        (PrettyTermInput(..), PrettyTermRule (..))

import           Component.Term.Int         (AsIntTerm (..), WithIntTerm)

-- |
prettyTmInt :: WithIntTerm tm n a
            => tm n a       -- ^
            -> Maybe Doc -- ^
prettyTmInt =
  fmap int .
  preview _TmIntLit

-- |
prettyTmAdd :: Doc
            -> Doc
            -> Doc
prettyTmAdd tm1 tm2 =
  tm1 <+>
  reservedOperator "+" <+>
  tm2

-- |
prettyTmSub :: Doc
            -> Doc
            -> Doc
prettyTmSub tm1 tm2 =
  tm1 <+>
  reservedOperator "-" <+>
  tm2

-- |
prettyTmMul :: Doc
            -> Doc
            -> Doc
prettyTmMul tm1 tm2 =
  tm1 <+>
  reservedOperator "*" <+>
  tm2

-- |
prettyTmExp :: Doc
            -> Doc
            -> Doc
prettyTmExp tm1 tm2 =
  tm1 <+>
  reservedOperator "^" <+>
  tm2

-- |
prettyTermInput :: WithIntTerm tm nTm a
                => PrettyTermInput ty nTy tm nTm a
prettyTermInput =
  PrettyTermInput
    [ PrettyTermBase prettyTmInt
    , PrettyTermExpression
      (ExpressionInfo AssocLeft 6)
      (preview _TmAdd)
      prettyTmAdd
    , PrettyTermExpression
      (ExpressionInfo AssocLeft 6)
      (preview _TmSub)
      prettyTmSub
    , PrettyTermExpression
      (ExpressionInfo AssocLeft 7)
      (preview _TmMul)
      prettyTmMul
    , PrettyTermExpression
      (ExpressionInfo AssocRight 8)
      (preview _TmExp)
      prettyTmExp
    ]
