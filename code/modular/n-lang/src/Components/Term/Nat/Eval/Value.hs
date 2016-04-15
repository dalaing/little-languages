{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Nat.Eval.Value (
    valueInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.Value (ValueRule(..), ValueInput(..))

import Components.Term.Nat (AsNatTerm(..), WithNatTerm)

valueTmZero :: WithNatTerm tm a
            => tm a
            -> Maybe (tm a)
valueTmZero =
  fmap (review _TmZero) .
  preview _TmZero

valueTmSucc :: WithNatTerm tm a
            => (tm a -> Maybe (tm a))
            -> tm a
            -> Maybe (tm a)
valueTmSucc value tm = do
  tm1 <- preview _TmSucc tm
  tm1' <- value tm1
  return $ review _TmSucc tm1'

valueInput :: WithNatTerm tm a
           => ValueInput (tm a)
valueInput =
  ValueInput
    [ ValueBase valueTmZero
    , ValueRecurse valueTmSucc
    ]
