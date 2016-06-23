{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Int.Eval.Value (
    valueInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.Value (ValueRule(..), ValueInput(..))

import Component.Term.Int (AsIntTerm(..), WithIntTerm)

valueTmInt :: WithIntTerm tm
            => tm nTy nTm a
            -> Maybe (tm nTy nTm a)
valueTmInt =
  fmap (review _TmIntLit) .
  preview _TmIntLit

valueInput :: WithIntTerm tm
           => ValueInput tm 
valueInput =
  ValueInput
    [ValueBase valueTmInt]
