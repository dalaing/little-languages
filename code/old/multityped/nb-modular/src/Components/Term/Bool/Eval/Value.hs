module Components.Term.Bool.Eval.Value (
    bv
  , valueInput
  ) where

import Data.Foldable (asum)

import Control.Lens (preview, review)

import Common.Recursion (Step(..), stepFnToReader)
import Common.Term.Eval.Value (ValueInput(..))

import Components.Term.Bool.Data

valueTmFalse :: WithBoolTerm ty tm
             => tm
             -> Maybe tm
valueTmFalse =
  fmap (review _TmFalse) .
  preview _TmFalse

valueTmTrue :: WithBoolTerm ty tm
             => tm
             -> Maybe tm
valueTmTrue =
  fmap (review _TmTrue) .
  preview _TmTrue

bv :: WithBoolTerm ty tm
   => tm
   -> Maybe tm
bv tm =
  asum .
  map ($ tm) $
    [ valueTmFalse
    , valueTmTrue
    ]

valueInput :: WithBoolTerm ty tm
           => ValueInput tm
valueInput =
  ValueInput .
  fmap stepFnToReader $
  [SBase bv]
