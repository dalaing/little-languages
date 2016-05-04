{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.STLC.Eval.Value (
    valueInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.Value (ValueRule(..), ValueInput(..))

import Component.Term.STLC (AsSTLCTerm(..), WithSTLCTerm)

valueTmLam :: WithSTLCTerm tm ty
           => tm nTy nTm a
           -> Maybe (tm nTy nTm a)
valueTmLam =
  fmap (review _TmLam) .
  preview _TmLam

valueInput :: WithSTLCTerm tm ty
           => ValueInput tm nTy nTm a
valueInput =
  ValueInput
   [ValueBase valueTmLam]
