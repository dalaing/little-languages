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

import Component.Term.Gen (GenAnyTermRule(..), ShrAnyTermRule(..), GenContainingTermRule(..), GenWellTypedTermRule(..), GenIllTypedTermRule(..), GenTermInput(..))

import           Component.Type.Nat (AsNatType (..), WithNatType)
import           Component.Term.Nat (AsNatTerm (..), WithNatTerm)

-- |
genTmZero :: WithNatTerm tm
          => Gen (tm n a)
genTmZero =
  pure $ review _TmZero ()

-- |
shrinkTmZero :: WithNatTerm tm
             => tm n a        -- ^
             -> Maybe [tm n a] -- ^
shrinkTmZero =
  fmap (const []) .
  preview _TmZero

-- |
genTmSucc :: WithNatTerm tm
          => Gen (tm n a)
          -> Gen (tm n a)
genTmSucc g =
  review _TmSucc <$> g

-- |
shrinkTmSucc :: WithNatTerm tm
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
genTmPred :: WithNatTerm tm
          => Gen (tm n a)
          -> Gen (tm n a)
genTmPred g =
  review _TmPred <$> g

-- |
shrinkTmPred :: WithNatTerm tm
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

genContainingTmZero :: ( WithNatType ty
                       , WithNatTerm tm
                       )
                    => tm nTm a
                    -> ty nTy
                    -> Maybe (Gen (tm nTm a))
genContainingTmZero tm ty = do
  _ <- preview _TmZero tm
  _ <- preview _TyNat ty
  return . pure $ review _TmZero ()

genContainingTmSucc :: ( WithNatType ty
                       , WithNatTerm tm
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmSucc _ genContaining tm ty s =
    fmap genContainingTmSucc' .
    preview _TyNat $
    ty
  where
    genContainingTmSucc' _ = do
      let s' = (s - 1) `max` 0
      tm1 <- genContaining tm (review _TyNat ()) s'
      return $ review _TmSucc tm1

genContainingTmPred :: ( WithNatType ty
                       , WithNatTerm tm
                       )
                    => (ty nTy -> Int -> Gen (tm nTm a))
                    -> (tm nTm a -> ty nTy -> Int -> Gen (tm nTm a))
                    -> tm nTm a
                    -> ty nTy
                    -> Int
                    -> Maybe (Gen (tm nTm a))
genContainingTmPred _ genContaining tm ty s =
    fmap genContainingTmPred' .
    preview _TyNat $
    ty
  where
    genContainingTmPred' _ = do
      let s' = (s - 1) `max` 0
      tm1 <- genContaining tm (review _TyNat ()) s'
      return $ review _TmPred tm1

genWellTypedTmZero :: ( WithNatType ty
                      , WithNatTerm tm
                      )
                   => ty nTy
                   -> Maybe (Gen (tm nTm a))
genWellTypedTmZero ty = do
  _ <- preview _TyNat ty
  return . pure $ review _TmZero ()

genWellTypedTmSucc :: ( WithNatType ty
                      , WithNatTerm tm
                      )
                   => (ty nTy -> Int -> Gen (tm nTm a))
                   -> ty nTy
                   -> Int
                   -> Maybe (Gen (tm nTm a))
genWellTypedTmSucc genWellTyped ty s = do
  _ <- preview _TyNat ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyNat ()) s'
    return $ review _TmSucc tm1

genWellTypedTmPred :: ( WithNatType ty
                      , WithNatTerm tm
                      )
                   => (ty nTy -> Int -> Gen (tm nTm a))
                   -> ty nTy
                   -> Int
                   -> Maybe (Gen (tm nTm a))
genWellTypedTmPred genWellTyped ty s = do
  _ <- preview _TyNat ty
  let s' = s `div` 2
  return $ do
    tm1 <- genWellTyped (review _TyNat ()) s'
    return $ review _TmPred tm1

genIllTypedTmSucc :: ( WithNatType ty
                     , WithNatTerm tm
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmSucc genNotType genWellTyped ty s = do
  _ <- preview _TyNat ty
  let s' = s `div` 2
  return $ do
    nty <- genNotType (review _TyNat ())
    tm1 <- genWellTyped nty s'
    return $ review _TmSucc tm1

genIllTypedTmPred :: ( WithNatType ty
                     , WithNatTerm tm
                     )
                  => (ty nTy -> Gen (ty nTy))
                  -> (ty nTy -> Int -> Gen (tm nTm a))
                  -> ty nTy
                  -> Int
                  -> Maybe (Gen (tm nTm a))
genIllTypedTmPred genNotType genWellTyped ty s = do
  _ <- preview _TyNat ty
  let s' = s `div` 2
  return $ do
    nty <- genNotType (review _TyNat ())
    tm1 <- genWellTyped nty s'
    return $ review _TmPred tm1

genTermInput :: ( WithNatTerm tm
                , WithNatType ty
                )
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
    [ GenContainingTermBase genContainingTmZero
    , GenContainingTermRecurse genContainingTmSucc
    , GenContainingTermRecurse genContainingTmPred
    ]
    [ GenWellTypedTermBase genWellTypedTmZero
    , GenWellTypedTermRecurse genWellTypedTmSucc
    , GenWellTypedTermRecurse genWellTypedTmPred
    ]
    [ GenIllTypedTermRecurse genIllTypedTmSucc
    , GenIllTypedTermRecurse genIllTypedTmPred
    ]
