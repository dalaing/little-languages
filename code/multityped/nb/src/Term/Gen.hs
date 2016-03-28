module Term.Gen where

import Control.Lens (preview)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe, mapMaybe)

import Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

import Type
import Type.Gen

import Term

genTmZero :: Gen Term
genTmZero =
  pure TmZero

genTmSucc :: Gen Term
          -> Gen Term
genTmSucc g =
  TmSucc <$> g

genTmPred :: Gen Term
          -> Gen Term
genTmPred g =
  TmPred <$> g

genTmFalse :: Gen Term
genTmFalse =
  pure TmFalse

genTmTrue :: Gen Term
genTmTrue =
  pure TmTrue

genTmIf :: Gen Term
        -> Gen Term
        -> Gen Term
        -> Gen Term
genTmIf g1 g2 g3 =
  TmIf <$> g1 <*> g2 <*> g3

genTmIsZero :: Gen Term
            -> Gen Term
genTmIsZero g =
  TmIsZero <$> g

genWellTypedTmZero :: Type
                   -> Maybe (Gen Term)
genWellTypedTmZero TyNat =
  Just genTmZero
genWellTypedTmZero _ =
  Nothing

genWellTypedTmSucc :: (Type -> Gen Term)
                   -> Type
                   -> Maybe (Gen Term)
genWellTypedTmSucc g TyNat =
  Just $ genTmSucc (g TyNat)
genWellTypedTmSucc _ _ =
  Nothing

genWellTypedTmPred :: (Type -> Gen Term)
                   -> Type
                   -> Maybe (Gen Term)
genWellTypedTmPred g TyNat =
  Just $ genTmPred (g TyNat)
genWellTypedTmPred _ _ =
  Nothing

genWellTypedTmFalse :: Type
                    -> Maybe (Gen Term)
genWellTypedTmFalse TyBool =
  Just genTmFalse
genWellTypedTmFalse _ =
  Nothing

genWellTypedTmTrue :: Type
                   -> Maybe (Gen Term)
genWellTypedTmTrue TyBool =
  Just genTmTrue
genWellTypedTmTrue _ =
  Nothing

genWellTypedTmIf :: (Type -> Gen Term)
                   -> Type
                   -> Maybe (Gen Term)
genWellTypedTmIf g t =
  Just $ genTmIf (g TyBool) (g t) (g t)

genWellTypedTmIsZero :: (Type -> Gen Term)
                   -> Type
                   -> Maybe (Gen Term)
genWellTypedTmIsZero g TyBool =
  Just $ genTmIsZero (g TyNat)
genWellTypedTmIsZero _ _ =
  Nothing

genWellTypedTerm :: Maybe Type
                 -> Gen (Type, Term)
genWellTypedTerm Nothing = do
  ty <- genType
  genWellTypedTerm (Just ty)
genWellTypedTerm (Just ty) =
  sized $ \s ->
    (,) ty <$> genWellTypedTerm' s ty

genWellTypedTerm' :: Int
                  -> Type
                  -> Gen Term
genWellTypedTerm' s t =
  let
    zeroSize =
      mapMaybe ($ t)
      [ genWellTypedTmZero
      , genWellTypedTmFalse
      , genWellTypedTmTrue
      ] 
    child = genWellTypedTerm' (s `div` 2)
    nonZeroSize =
      mapMaybe (\x -> x child t)
      [ genWellTypedTmSucc
      , genWellTypedTmPred
      , genWellTypedTmIf
      , genWellTypedTmIsZero
      ]
  in
    oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

-- unexpected, ac: Bool, ex : Nat
genIllTypedTmSucc :: (Type -> Gen Term)
                   -> Type
                   -> Maybe (Gen Term)
genIllTypedTmSucc g TyNat =
  Just $ genTmSucc (g TyBool)
genIllTypedTmSucc _ _ =
  Nothing

-- unexpected, ac: Bool, ex : Nat
genIllTypedTmPred :: (Type -> Gen Term)
                   -> Type
                   -> Maybe (Gen Term)
genIllTypedTmPred g TyNat =
  Just $ genTmPred (g TyBool)
genIllTypedTmPred _ _ =
  Nothing

  -- unexpected
  -- if not-bool any any
  -- if bool not-t not-t
  -- expectedeq
  -- if bool not-t t
  -- if bool t not-t
genIllTypedTmIf :: (Type -> Gen Term)
                   -> Type
                   -> Maybe (Gen Term)
genIllTypedTmIf g t =
  Just . oneof . map (\x -> x g t) $
  [ genIllTypedTmIf1
  , genIllTypedTmIf2
  , genIllTypedTmIf3
  ]

  -- unexpected
  -- if not-bool any any
genIllTypedTmIf1 :: (Type -> Gen Term)
                   -> Type
                   -> Gen Term
genIllTypedTmIf1 g _ = do
  tb <- genNotType TyBool
  ty <- genType
  genTmIf (g tb) (g ty) (g ty)

  -- expectedeq
  -- if bool not-t t
genIllTypedTmIf2 :: (Type -> Gen Term)
                   -> Type
                   -> Gen Term
genIllTypedTmIf2 g t = do
  nt <- genNotType t
  genTmIf (g TyBool) (g nt) (g t)

  -- expectedeq
  -- if bool t not-t
genIllTypedTmIf3 :: (Type -> Gen Term)
                   -> Type
                   -> Gen Term
genIllTypedTmIf3 g t = do
  nt <- genNotType t
  genTmIf (g TyBool) (g t) (g nt)

-- expected nat actual bool
genIllTypedTmIsZero :: (Type -> Gen Term)
                    -> Type
                    -> Maybe (Gen Term)
genIllTypedTmIsZero g TyBool =
  Just $ genTmIsZero (g TyBool)
genIllTypedTmIsZero _ _ =
  Nothing

-- what's the best API to marginalize over
-- - output types
-- - type errors

genIllTypedTerm :: Maybe Type
                -> Gen (Type, Term)
genIllTypedTerm Nothing = do
  ty <- genType
  genIllTypedTerm (Just ty)
genIllTypedTerm (Just ty) =
  sized $ \s ->
    (,) ty <$> genIllTypedTerm' s ty

genIllTypedTerm' :: Int
                 -> Type
                 -> Gen Term
genIllTypedTerm' s t =
  let
    child = genWellTypedTerm' (s `div` 2)
    nonZeroSize =
      mapMaybe (\x -> x child t)
      [ genIllTypedTmSucc
      , genIllTypedTmPred
      , genIllTypedTmIf
      , genIllTypedTmIsZero
      ]
  in
    oneof nonZeroSize


genContainingTmSucc :: (Type -> (Type, Term) -> Gen Term)
                    -> Type
                    -> (Type, Term)
                    -> Maybe (Gen Term)
genContainingTmSucc g TyNat p =
  Just $ genTmSucc (g TyNat p)
genContainingTmSucc _ _ _ =
  Nothing

genContainingTmPred :: (Type -> (Type, Term) -> Gen Term)
                    -> Type
                    -> (Type, Term)
                    -> Maybe (Gen Term)
genContainingTmPred g TyNat p =
  Just $ genTmPred (g TyNat p)
genContainingTmPred _ _ _ =
  Nothing

genContainingTmIf :: (Type -> Gen Term)
                  -> (Type -> (Type, Term) -> Gen Term)
                  -> Type
                  -> (Type, Term)
                  -> Maybe (Gen Term)
genContainingTmIf g gt tyT p =
  let
    branch = genTmIf (gt TyBool p) (g tyT) (g tyT)
    arm1 = genTmIf (g TyBool) (gt tyT p) (g tyT)
    arm2 = genTmIf (g TyBool) (g tyT) (gt tyT p)
  in
    Just . oneof $ [branch, arm1, arm2]

genContainingTmIsZero :: (Type -> (Type, Term) -> Gen Term)
                      -> Type
                      -> (Type, Term)
                      -> Maybe (Gen Term)
genContainingTmIsZero g TyBool p =
  Just $ genTmIsZero (g TyNat p)
genContainingTmIsZero _ _ _ =
  Nothing

genTypedTermContaining :: Maybe Type
                       -> (Type, Term)
                       -> Gen (Type, Term)
genTypedTermContaining Nothing p = do
  ty <- genType
  genTypedTermContaining (Just ty) p
genTypedTermContaining (Just ty) p =
  sized $ \s ->
    (,) ty <$> genTypedTermContaining' s ty p

genTypedTermContaining' :: Int
                        -> Type
                        -> (Type, Term)
                        -> Gen Term
genTypedTermContaining' s tyT (tyS, tmS)
  | s == 0 && tyT == tyS = pure tmS
  | otherwise =
    let
      s' = s `div` 2
      child = genTypedTermContaining' s'
      other = genWellTypedTerm' s'
    in
      oneof $
      (if tyT == tyS then [pure tmS] else []) ++
      mapMaybe (\x -> x child tyT (tyS, tmS))
      [ genContainingTmSucc
      , genContainingTmPred
      , genContainingTmIf other
      , genContainingTmIsZero
      ]

genContainingIllTypedTerm :: Maybe Type
                -> Gen (Type, Term)
genContainingIllTypedTerm ty = do
  ill <- genContainingIllTypedTerm Nothing
  genTypedTermContaining ty ill

-- do we break ill-typed down to the granularity of typerrors
-- that might require genTermContaining, at least for STLC
-- is our input then
--   Either (TypeError l) (Type l)
-- do we expand the rules to explicitly create errors of this form?
-- we can probably demonstrate that the use of expect / expectEq do
--   what we want in this case
-- is there a correspondence to the inversion lemmas?
-- would be nice to try to generate as many errors as we can,
-- so that people don't have to play whack-a-mole with them

-- we should break this out, like the pieces of genTerm
-- we should probably do the same thing for genWellTypedTerm and genIllTypedTerm
-- do we go the whole way down to specific type errors for illtyped term?

genAnyTerm :: Gen Term
genAnyTerm = sized genTerm'
  where
    genTerm' s =
      let
        zeroSize = [genTmZero, genTmFalse, genTmTrue]
        child = genTerm' (s `div` 2)
        nonZeroSize = [genTmSucc child, genTmPred child, genTmIf child child child, genTmIsZero child]
      in
        oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

shrTmZero :: Term -> Maybe [Term]
shrTmZero = fmap (const []) . preview _TmZero

shrTmSucc :: (Term -> [Term]) -> Term -> Maybe [Term]
shrTmSucc shr = fmap shrTmSucc' . preview _TmSucc
  where
    shrTmSucc' t =
      shr t ++
      fmap TmSucc (shr t)

shrTmPred :: (Term -> [Term]) -> Term -> Maybe [Term]
shrTmPred shr = fmap shrTmPred' . preview _TmPred
  where
    shrTmPred' t =
      shr t ++
      fmap TmPred (shr t)

shrTmFalse :: Term -> Maybe [Term]
shrTmFalse = fmap (const []) . preview _TmFalse

shrTmTrue :: Term -> Maybe [Term]
shrTmTrue = fmap (const []) . preview _TmTrue

shrTmIf :: (Term -> [Term]) -> (Term -> [Term]) -> (Term -> [Term]) -> Term -> Maybe [Term]
shrTmIf s1 s2 s3 = fmap shrTmIf' . preview _TmIf
  where
    shrTmIf' (t1, t2, t3) =
      s2 t2 ++
      s3 t3 ++
      fmap (\u1 -> TmIf u1 t2 t3) (s1 t1) ++
      fmap (\u2 -> TmIf t1 u2 t3) (s2 t2) ++
      fmap (\u3 -> TmIf t1 t2 u3) (s3 t3)

shrTmIsZero :: (Term -> [Term]) -> Term -> Maybe [Term]
shrTmIsZero shr = fmap shrTmIsZero' . preview _TmIsZero
  where
    shrTmIsZero' t =
      fmap TmIsZero (shr t)

shrinkTerm :: Term -> [Term]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    shrTmZero
  , shrTmSucc shrinkTerm
  , shrTmPred shrinkTerm
  , shrTmFalse
  , shrTmTrue
  , shrTmIf shrinkTerm shrinkTerm shrinkTerm
  , shrTmIsZero shrinkTerm
  ]

-- TODO use pretty printer for the show here
newtype AnyTerm = AnyTerm { getAnyTerm :: Term }
                deriving (Eq, Ord, Show)

instance Arbitrary AnyTerm where
  arbitrary = AnyTerm <$> genAnyTerm
  shrink = fmap AnyTerm . shrinkTerm . getAnyTerm

newtype WellTypedTerm = WellTypedTerm { getWellTypedTerm :: Term }
                      deriving (Eq, Ord, Show)

instance Arbitrary WellTypedTerm where
  arbitrary = (WellTypedTerm . snd) <$> genWellTypedTerm Nothing
  shrink = fmap WellTypedTerm . shrinkTerm . getWellTypedTerm

newtype IllTypedTerm = IllTypedTerm { getIllTypedTerm :: Term }
                      deriving (Eq, Ord, Show)

instance Arbitrary IllTypedTerm where
  arbitrary = (IllTypedTerm . snd) <$> genIllTypedTerm Nothing
  shrink = fmap IllTypedTerm . shrinkTerm . getIllTypedTerm
