{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Bool.Eval.Value (
    valueInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.Value (ValueRule(..), ValueInput(..))

import Components.Term.Bool (AsBoolTerm(..), WithBoolTerm)

valueTmFalse :: WithBoolTerm tm a
             => tm a
             -> Maybe (tm a)
valueTmFalse =
  fmap (review _TmFalse) .
  preview _TmFalse

valueTmTrue :: WithBoolTerm tm a
            => tm a
            -> Maybe (tm a)
valueTmTrue =
  fmap (review _TmTrue) .
  preview _TmTrue

valueInput :: WithBoolTerm tm a
           => ValueInput (tm a)
valueInput =
  ValueInput
    [ ValueBase valueTmFalse
    , ValueBase valueTmTrue
    ]
