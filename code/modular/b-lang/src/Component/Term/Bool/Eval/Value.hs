{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Bool.Eval.Value (
    valueInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.Value (ValueRule(..), ValueInput(..))

import Component.Term.Bool (AsBoolTerm(..), WithBoolTerm)

valueTmFalse :: WithBoolTerm tm
             => tm nTy nTm a
             -> Maybe (tm nTy nTm a)
valueTmFalse =
  fmap (review _TmFalse) .
  preview _TmFalse

valueTmTrue :: WithBoolTerm tm
            => tm nTy nTm a
            -> Maybe (tm nTy nTm a)
valueTmTrue =
  fmap (review _TmTrue) .
  preview _TmTrue

valueInput :: WithBoolTerm tm
           => ValueInput tm
valueInput =
  ValueInput
    [ ValueBase valueTmFalse
    , ValueBase valueTmTrue
    ]
