{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Nat.Gen (
    genTermInput
  ) where

import           Control.Lens         (preview, review)
import           Test.QuickCheck      (Gen)

import Component.Term.Gen (GenAnyTermRule(..), ShrAnyTermRule(..), GenTermInput(..))

import           Components.Term.Nat (AsNatTerm (..), WithNatTerm)

-- |
genTmZero :: WithNatTerm tm a
          => Gen (tm a)
genTmZero =
  pure $ review _TmZero ()

-- |
shrinkTmZero :: WithNatTerm tm a
             => tm a        -- ^
             -> Maybe [tm a] -- ^
shrinkTmZero =
  fmap (const []) .
  preview _TmZero

-- |
genTmSucc :: WithNatTerm tm a
          => Gen (tm a)
          -> Gen (tm a)
genTmSucc g =
  review _TmSucc <$> g

-- |
shrinkTmSucc :: WithNatTerm tm a
             => (tm a -> [tm a])
             -> tm a        -- ^
             -> Maybe [tm a] -- ^
shrinkTmSucc s =
    fmap shrinkTmSucc' .
    preview _TmSucc
  where
    shrinkTmSucc' tm =
      s tm ++ [tm] ++
      fmap (review _TmSucc) (s tm)

-- |
genTmPred :: WithNatTerm tm a
          => Gen (tm a)
          -> Gen (tm a)
genTmPred g =
  review _TmPred <$> g

-- |
shrinkTmPred :: WithNatTerm tm a
             => (tm a -> [tm a])
             -> tm a         -- ^
             -> Maybe [tm a] -- ^
shrinkTmPred s =
    fmap shrinkTmPred' .
    preview _TmPred
  where
    shrinkTmPred' tm =
      s tm ++ [tm] ++
      fmap (review _TmPred) (s tm)

genTermInput :: WithNatTerm tm a
             => GenTermInput (tm a)
genTermInput =
  GenTermInput
    [ GenAnyTermBase genTmZero
    , GenAnyTermRecurse $ \g s ->
        let
          child = g (s - 1)
        in
          genTmSucc child
    , GenAnyTermRecurse $ \g s ->
        let
          child = g (s - 1)
        in
          genTmPred child
    ]
    [ ShrAnyTermBase shrinkTmZero
    , ShrAnyTermRecurse shrinkTmSucc
    , ShrAnyTermRecurse shrinkTmPred
    ]
