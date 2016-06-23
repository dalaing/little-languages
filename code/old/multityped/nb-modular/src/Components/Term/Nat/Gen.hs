{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Components.Term.Nat.Gen where

import Control.Lens (preview, review)

import Test.QuickCheck (Gen)

import Common.Term.Gen

import Components.Type.Nat.Data
import Components.Term.Nat.Data

genTmZero :: WithNatTerm ty tm
          => Gen tm
genTmZero =
  pure $ review _TmZero ()

genTmSucc :: WithNatTerm ty tm
          => Gen tm
          -> Gen tm
genTmSucc g =
  review _TmSucc <$> g

genTmPred :: WithNatTerm ty tm
          => Gen tm
          -> Gen tm
genTmPred g =
  review _TmPred <$> g

genWellTypedTmZero :: WithNatTerm ty tm
                   => ty
                   -> Maybe (Gen tm)
genWellTypedTmZero =
  fmap (const genTmZero) .
  preview _TyNat

genWellTypedTmSucc :: WithNatTerm ty tm
                   => (ty -> Gen tm)
                   -> ty
                   -> Maybe (Gen tm)
genWellTypedTmSucc genTypedTerm =
    fmap (const genWellTypedTmSucc') .
    preview _TyNat
  where
    genWellTypedTmSucc' =
      genTmSucc (genTypedTerm (review _TyNat ()))

genWellTypedTmPred :: WithNatTerm ty tm
                   => (ty -> Gen tm)
                   -> ty
                   -> Maybe (Gen tm)
genWellTypedTmPred genTypedTerm =
    fmap (const genWellTypedTmPred') .
    preview _TyNat
  where
    genWellTypedTmPred' =
      genTmPred (genTypedTerm (review _TyNat ()))

-- unexpected, ac: Bool, ex : Nat
genIllTypedTmSucc :: WithNatTerm ty tm
                  => Gen ty
                  -> (ty -> Gen ty)
                  -> (ty -> Gen tm)
                  -> ty
                  -> Maybe (Gen tm)
genIllTypedTmSucc _ genNotType genTypedTerm =
    fmap (const genIllTypedTmSucc') .
    preview _TyNat
  where
    genIllTypedTmSucc' = do
      nt <- genNotType (review _TyNat ())
      genTmSucc (genTypedTerm nt)

genIllTypedTmPred :: WithNatTerm ty tm
                  => Gen ty
                  -> (ty -> Gen ty)
                  -> (ty -> Gen tm)
                  -> ty
                  -> Maybe (Gen tm)
genIllTypedTmPred _ genNotType genTypedTerm =
    fmap (const genIllTypedTmPred') .
    preview _TyNat
  where
    genIllTypedTmPred' = do
      nt <- genNotType (review _TyNat ())
      genTmPred (genTypedTerm nt)

shrTmZero :: WithNatTerm ty tm
          => tm
          -> Maybe [tm]
shrTmZero =
  fmap (const []) .
  preview _TmZero

shrTmSucc :: WithNatTerm ty tm
          => (tm -> [tm])
          -> tm
          -> Maybe [tm]
shrTmSucc shr =
    fmap shrTmSucc' .
    preview _TmSucc
  where
    shrTmSucc' t =
      shr t ++
      [t] ++
      fmap (review _TmSucc) (shr t)

shrTmPred :: WithNatTerm ty tm
          => (tm -> [tm])
          -> tm
          -> Maybe [tm]
shrTmPred shr =
    fmap shrTmPred' .
    preview _TmPred
  where
    shrTmPred' t =
      shr t ++
      [t] ++
      fmap (review _TmPred) (shr t)

genTermInput :: WithNatTerm ty tm
             => GenTermInput ty tm
genTermInput =
  GenTermInput
    [ ABase genTmZero
    , ARecurse genTmSucc
    , ARecurse genTmPred
    ]
    [ WtBase genWellTypedTmZero
    , WtRecurse genWellTypedTmSucc
    , WtRecurse genWellTypedTmPred
    ]
    [ ItRecurse genIllTypedTmSucc
    , ItRecurse genIllTypedTmPred
    ]
    [ ShrTmBase shrTmZero
    , ShrTmRecurse shrTmSucc
    , ShrTmRecurse shrTmPred
    ]
