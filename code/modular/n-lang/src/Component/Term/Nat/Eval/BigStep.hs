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
eZero :: WithNatTerm tm
      => tm nTy nTm a             -- ^
      -> Maybe (tm nTy nTm a)        -- ^
eZero =
  fmap (review _TmZero) .
  preview _TmZero

-- |
eSucc :: WithNatTerm tm
      => (tm nTy nTm a -> Maybe (tm nTy nTm a))
      -> tm nTy nTm a
      -> Maybe (tm nTy nTm a)
eSucc step tm = do
  t <- preview _TmSucc tm
  t' <- step t
  return $ review _TmSucc t'

-- |
ePredZero :: WithNatTerm tm
          => (tm nTy nTm a -> Maybe (tm nTy nTm a))
          -> tm nTy nTm a
          -> Maybe (tm nTy nTm a)
ePredZero step tm = do
  t <- preview _TmPred tm
  t' <- step t
  preview _TmZero t'
  return $ review _TmZero ()

-- |
ePredSucc :: WithNatTerm tm
          => (tm nTy nTm a -> Maybe (tm nTy nTm a))
          -> tm nTy nTm a
          -> Maybe (tm nTy nTm a)
ePredSucc step tm = do
  t <- preview _TmPred tm
  t' <- step t
  preview _TmSucc t'

-- |
bigStepInput :: WithNatTerm tm
             => BigStepInput tm nTy nTm a
bigStepInput =
  BigStepInput
    [ BigStepBase eZero
    , BigStepRecurse eSucc
    , BigStepRecurse ePredZero
    , BigStepRecurse ePredSucc
    ]
