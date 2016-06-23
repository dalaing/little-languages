module Components.Term.NatBool.Gen where

import Control.Lens (preview, review)

import Test.QuickCheck (Gen)

import Common.Term.Gen

import Components.Type.Nat.Data
import Components.Type.Bool.Data
import Components.Term.NatBool.Data

genTmIsZero :: WithNatBoolTerm ty tm
            => Gen tm
            -> Gen tm
genTmIsZero g =
  review _TmIsZero <$> g

genWellTypedTmIsZero :: WithNatBoolTerm ty tm
                     => (ty -> Gen tm)
                   -> ty
                   -> Maybe (Gen tm)
genWellTypedTmIsZero genTypedTerm =
    fmap (const genWellTypedTmIsZero') .
    preview _TyBool
  where
    genWellTypedTmIsZero' =
      genTmIsZero (genTypedTerm (review _TyNat ()))

-- expected nat actual bool
genIllTypedTmIsZero :: WithNatBoolTerm ty tm
                    => Gen ty
                    -> (ty -> Gen ty)
                    -> (ty -> Gen tm)
                    -> ty
                    -> Maybe (Gen tm)
genIllTypedTmIsZero _ genNotType genTypedTerm =
    fmap (const genIllTypedTmIsZero') .
    preview _TyBool
  where
    genIllTypedTmIsZero' = do
      nt <- genNotType (review _TyNat ())
      genTmIsZero (genTypedTerm nt)

shrTmIsZero :: WithNatBoolTerm ty tm
            => (tm -> [tm])
            -> tm
            -> Maybe [tm]
shrTmIsZero shr =
    fmap shrTmIsZero' .
    preview _TmIsZero
  where
    shrTmIsZero' t =
      fmap (review _TmIsZero) (shr t)

genTermInput :: WithNatBoolTerm ty tm
             => GenTermInput ty tm
genTermInput =
  GenTermInput
    [ARecurse genTmIsZero]
    [WtRecurse genWellTypedTmIsZero]
    [ItRecurse genIllTypedTmIsZero]
    [ShrTmRecurse shrTmIsZero]
