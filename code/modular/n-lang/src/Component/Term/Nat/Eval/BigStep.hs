{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Nat.Eval.BigStep (
    bigStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.BigStep (BigStepRule(..), BigStepInput(..))

import Component.Term.Nat (AsNatTerm(..), WithNatTerm)

-- |
eZero :: WithNatTerm tm n a
      => tm n a             -- ^
      -> Maybe (tm n a)        -- ^
eZero =
  fmap (review _TmZero) .
  preview _TmZero

-- |
eSucc :: WithNatTerm tm n a
      => (tm n a -> Maybe (tm n a))
      -> tm n a
      -> Maybe (tm n a)
eSucc step tm = do
  t <- preview _TmSucc tm
  t' <- step t
  return $ review _TmSucc t'

-- |
ePredZero :: WithNatTerm tm n a
          => (tm n a -> Maybe (tm n a))
          -> tm n a
          -> Maybe (tm n a)
ePredZero step tm = do
  t <- preview _TmPred tm
  t' <- step t
  preview _TmZero t'
  return $ review _TmZero ()

-- |
ePredSucc :: WithNatTerm tm n a
          => (tm n a -> Maybe (tm n a))
          -> tm n a
          -> Maybe (tm n a)
ePredSucc step tm = do
  t <- preview _TmPred tm
  t' <- step t
  preview _TmSucc t'

-- |
bigStepInput :: WithNatTerm tm n a
             => BigStepInput tm n a
bigStepInput =
  BigStepInput
    [ BigStepBase eZero
    , BigStepRecurse eSucc
    , BigStepRecurse ePredZero
    , BigStepRecurse ePredSucc
    ]
