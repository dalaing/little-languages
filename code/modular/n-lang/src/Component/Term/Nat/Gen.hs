{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Nat.Gen (
    genTermInput
  ) where

import           Control.Lens         (preview, review)
import           Test.QuickCheck      (Gen)

import Component.Term.Gen (GenAnyTermRule(..), ShrAnyTermRule(..), GenTermInput(..))

import           Component.Term.Nat (AsNatTerm (..), WithNatTerm)

-- |
genTmZero :: WithNatTerm tm n a
          => Gen (tm n a)
genTmZero =
  pure $ review _TmZero ()

-- |
shrinkTmZero :: WithNatTerm tm n a
             => tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmZero =
  fmap (const []) .
  preview _TmZero

-- |
genTmSucc :: WithNatTerm tm n a
          => Gen (tm n a)
          -> Gen (tm n a)
genTmSucc g =
  review _TmSucc <$> g

-- |
shrinkTmSucc :: WithNatTerm tm n a
             => (tm n a -> [tm n a])
             -> tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmSucc s =
    fmap shrinkTmSucc' .
    preview _TmSucc
  where
    shrinkTmSucc' tm =
      s tm ++ [tm] ++
      fmap (review _TmSucc) (s tm)

-- |
genTmPred :: WithNatTerm tm n a
          => Gen (tm n a)
          -> Gen (tm n a)
genTmPred g =
  review _TmPred <$> g

-- |
shrinkTmPred :: WithNatTerm tm n a
             => (tm n a -> [tm n a])
             -> tm n a         -- ^
             -> Maybe [tm n a] -- ^
shrinkTmPred s =
    fmap shrinkTmPred' .
    preview _TmPred
  where
    shrinkTmPred' tm =
      s tm ++ [tm] ++
      fmap (review _TmPred) (s tm)

genTermInput :: WithNatTerm tm nTm a
             => GenTermInput ty nTy tm nTm a
genTermInput =
  GenTermInput
    [ GenAnyTermBase genTmZero
    , GenAnyTermRecurse $ \ g s -> genTmSucc (g (s - 1))
    , GenAnyTermRecurse $ \ g s -> genTmPred (g (s - 1))
    ]
    [ ShrAnyTermBase shrinkTmZero
    , ShrAnyTermRecurse shrinkTmSucc
    , ShrAnyTermRecurse shrinkTmPred
    ]
    mempty
    mempty
    mempty
