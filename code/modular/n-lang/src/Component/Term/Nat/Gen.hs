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

import Component.Term.Gen (GenAnyTermRule(..), ShrAnyTermRule(..), GenTermInput(..), GenValueTermRule(..), ShrValueTermRule(..), GenNonValueTermRule(..), ShrNonValueTermRule(..))

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

-- |
genValueTmZero :: WithNatTerm tm n a
          => Gen (tm n a)
genValueTmZero =
  genTmZero

-- |
shrValueTmZero :: WithNatTerm tm n a
               => tm n a        -- ^
               -> Maybe [tm n a] -- ^
shrValueTmZero =
  shrinkTmZero

-- |
genValueTmSucc :: WithNatTerm tm n a
               => (Int -> Gen (tm n a))
               -> Int
               -> Gen (tm n a)
genValueTmSucc g s =
  genTmSucc (g (s - 1))

-- |
shrValueTmSucc :: WithNatTerm tm n a
               => (tm n a -> [tm n a])
               -> tm n a        -- ^
               -> Maybe [tm n a] -- ^
shrValueTmSucc s =
    fmap shrinkValueTmSucc' .
    preview _TmSucc
  where
    shrinkValueTmSucc' tm =
      fmap (review _TmSucc) (s tm)

-- |
genNonValueTmPred :: WithNatTerm tm n a
                  => (Int -> Gen (tm n a))
                  -> Int
                  -> Gen (tm n a)
genNonValueTmPred g s =
  genTmPred (g (s - 1))

-- |
shrNonValueTmPred :: WithNatTerm tm n a
                  => (tm n a -> [tm n a])
                  -> tm n a        -- ^
                 -> Maybe [tm n a] -- ^
shrNonValueTmPred s =
    fmap shrinkNonValueTmPred' .
    preview _TmPred
  where
    shrinkNonValueTmPred' tm =
      fmap (review _TmPred) (s tm)

genTermInput :: WithNatTerm tm n a
             => GenTermInput tm n a
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
    [ GenValueTermBase genValueTmZero
    , GenValueTermRecurse genValueTmSucc
    ]
    [ ShrValueTermBase shrValueTmZero
    , ShrValueTermRecurse shrValueTmSucc
    ]
    [ GenNonValueTermRecurse genNonValueTmPred
    ]
    [ ShrNonValueTermRecurse shrNonValueTmPred
    ]
