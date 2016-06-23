module Components.Term.Bool.Gen where

import Control.Lens (preview, review)

import Test.QuickCheck (Gen, oneof)

import Common.Term.Gen

import Components.Type.Bool.Data
import Components.Term.Bool.Data

genTmFalse :: WithBoolTerm ty tm
           => Gen tm
genTmFalse =
  pure $ review _TmFalse ()

genTmTrue :: WithBoolTerm ty tm
          => Gen tm
genTmTrue =
  pure $ review _TmTrue ()

genTmIf :: WithBoolTerm ty tm
        => Gen tm
        -> Gen tm
        -> Gen tm
        -> Gen tm
genTmIf g1 g2 g3 =
  fmap (review _TmIf) ((,,) <$> g1 <*> g2 <*> g3)

genWellTypedTmFalse :: WithBoolTerm ty tm
                    => ty
                    -> Maybe (Gen tm)
genWellTypedTmFalse =
  fmap (const genTmFalse) .
  preview _TyBool

genWellTypedTmTrue :: WithBoolTerm ty tm
                   => ty
                   -> Maybe (Gen tm)
genWellTypedTmTrue =
  fmap (const genTmTrue) .
  preview _TyBool

genWellTypedTmIf :: WithBoolTerm ty tm
                 => (ty -> Gen tm)
                 -> ty
                 -> Maybe (Gen tm)
genWellTypedTmIf genTypedTerm ty =
  Just $ genTmIf (genTypedTerm (review _TyBool ())) (genTypedTerm ty) (genTypedTerm ty)

genIllTypedTmIf :: WithBoolTerm ty tm
                => Gen ty
                -> (ty -> Gen ty)
                -> (ty -> Gen tm)
                -> ty
                -> Maybe (Gen tm)
genIllTypedTmIf genType genNotType genTypedTerm ty =
  Just . oneof . map (\x -> x genType genNotType genTypedTerm ty) $
  [ genIllTypedTmIf1
  , genIllTypedTmIf2
  , genIllTypedTmIf3
  ]

  -- unexpected
  -- if not-bool any any
genIllTypedTmIf1 :: WithBoolTerm ty tm
                 => Gen ty
                 -> (ty -> Gen ty)
                 -> (ty -> Gen tm)
                 -> ty
                 -> Gen tm
genIllTypedTmIf1 genType genNotType genTypedTerm _ = do
  tb <- genNotType (review _TyBool ())
  ty <- genType
  genTmIf (genTypedTerm tb) (genTypedTerm ty) (genTypedTerm ty)

  -- expectedeq
  -- if bool not-t t
genIllTypedTmIf2 :: WithBoolTerm ty tm
                 => Gen ty
                 -> (ty -> Gen ty)
                 -> (ty -> Gen tm)
                 -> ty
                 -> Gen tm
genIllTypedTmIf2 _ genNotType genTypedTerm ty = do
  nt <- genNotType ty
  genTmIf (genTypedTerm (review _TyBool ())) (genTypedTerm nt) (genTypedTerm ty)

  -- expectedeq
  -- if bool t not-t
genIllTypedTmIf3 :: WithBoolTerm ty tm
                 => Gen ty
                 -> (ty -> Gen ty)
                 -> (ty -> Gen tm)
                 -> ty
                 -> Gen tm
genIllTypedTmIf3 _ genNotType genTypedTerm ty = do
  nt <- genNotType ty
  genTmIf (genTypedTerm (review _TyBool ())) (genTypedTerm ty) (genTypedTerm nt)

shrTmFalse :: WithBoolTerm ty tm
           => tm
           -> Maybe [tm]
shrTmFalse =
  fmap (const []) .
  preview _TmFalse

shrTmTrue :: WithBoolTerm ty tm
          => tm
          -> Maybe [tm]
shrTmTrue =
  fmap (const []) .
  preview _TmTrue

shrTmIf :: WithBoolTerm ty tm
        => (tm -> [tm])
        -> tm
        -> Maybe [tm]
shrTmIf shr =
    fmap shrTmIf' .
    preview _TmIf
  where
    shrTmIf' (t1, t2, t3) =
      shr t2 ++
      [t2] ++
      shr t3 ++
      [t3] ++
      fmap (\u1 -> review _TmIf (u1, t2, t3)) (shr t1) ++
      fmap (\u2 -> review _TmIf (t1, u2, t3)) (shr t2) ++
      fmap (\u3 -> review _TmIf (t1, t2, u3)) (shr t3)

genTermInput :: WithBoolTerm ty tm
             => GenTermInput ty tm
genTermInput =
  GenTermInput
    [ ABase genTmFalse
    , ABase genTmTrue
    , ARecurse $ \c -> genTmIf c c c
    ]
    [ WtBase genWellTypedTmFalse
    , WtBase genWellTypedTmTrue
    , WtRecurse genWellTypedTmIf
    ]
    [ItRecurse genIllTypedTmIf]
    [ ShrTmBase shrTmFalse
    , ShrTmBase shrTmTrue
    , ShrTmRecurse shrTmIf
    ]
