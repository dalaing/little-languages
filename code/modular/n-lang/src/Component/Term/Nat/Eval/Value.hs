{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Nat.Eval.Value (
    valueInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.Value (ValueRule(..), ValueInput(..))

import Component.Term.Nat (AsNatTerm(..), WithNatTerm)

valueTmZero :: WithNatTerm tm n a
            => tm n a
            -> Maybe (tm n a)
valueTmZero =
  fmap (review _TmZero) .
  preview _TmZero

valueTmSucc :: WithNatTerm tm n a
            => (tm n a -> Maybe (tm n a))
            -> tm n a
            -> Maybe (tm n a)
valueTmSucc value tm = do
  tm1 <- preview _TmSucc tm
  tm1' <- value tm1
  return $ review _TmSucc tm1'

valueInput :: WithNatTerm tm n a
           => ValueInput tm n a
valueInput =
  ValueInput
    [ ValueBase valueTmZero
    , ValueRecurse valueTmSucc
    ]
