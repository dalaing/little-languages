{-# LANGUAGE FlexibleContexts #-}
module Term.Gen where

import Control.Lens (preview, review)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Text.Trifecta.Rendering (Span)

import Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

import Type
import Type.Gen

import Loc
import Term

genTmZero :: Gen (Term l)
genTmZero = pure (review _TmZero ())

genTmSucc :: Gen (Term l) -> Gen (Term l)
genTmSucc g = review _TmSucc <$> g

genTmPred :: Gen (Term l) -> Gen (Term l)
genTmPred g = review _TmPred <$> g

genTmFalse :: Gen (Term l)
genTmFalse = pure (review _TmFalse ())

genTmTrue :: Gen (Term l)
genTmTrue = pure (review _TmTrue ())

genTmIf :: Gen (Term l) -> Gen (Term l) -> Gen (Term l) -> Gen (Term l)
genTmIf g1 g2 g3 = fmap (review _TmIf) ((,,) <$> g1 <*> g2 <*> g3)

genTmIsZero :: Gen (Term l) -> Gen (Term l)
genTmIsZero g = review _TmIsZero <$> g

genAnyTerm :: Gen (Term l)
genAnyTerm = sized genTerm'
  where
    genTerm' s =
      let
        zeroSize = [genTmZero, genTmFalse, genTmTrue]
        child = genTerm' (s `div` 2)
        nonZeroSize = [genTmSucc child, genTmPred child, genTmIf child child child, genTmIsZero child]
      in
        oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

genWellTypedTerm :: Maybe (Type l) -> Gen (Term l)
genWellTypedTerm ty = sized (genWellTypedTerm' ty)

genWellTypedTerm' :: Maybe (Type l) -> Int -> Gen (Term l)
genWellTypedTerm' Nothing s = do
  ty <- genType
  genWellTypedTerm' (Just ty) s
genWellTypedTerm' (Just TyBool) s =
  let
    zeroSize = [genTmFalse, genTmTrue]
    childB = genWellTypedTerm' (Just TyBool) (s `div` 2)
    childN = genWellTypedTerm' (Just TyNat) (s `div` 2)
    nonZeroSize = [genTmIsZero childN, genTmIf childB childB childB]
  in
    oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize
genWellTypedTerm' (Just TyNat) s =
  let
    zeroSize = [genTmZero]
    childB = genWellTypedTerm' (Just TyBool) (s `div` 2)
    childN = genWellTypedTerm' (Just TyNat) (s `div` 2)
    nonZeroSize = [genTmSucc childN, genTmPred childN, genTmIf childB childN childN]
  in
    oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

genIllTypedTerm :: Maybe (Type l) -> Gen (Term l)
genIllTypedTerm ty = sized (genIllTypedTerm' ty)

-- this will produce a term that is ill typed at the outermost constructor
-- use genTermContaining to nest it elsewhere?
genIllTypedTerm' :: Maybe (Type l) -> Int -> Gen (Term l)
genIllTypedTerm' Nothing s = do
  ty <- genType
  genIllTypedTerm' (Just ty) s
genIllTypedTerm' (Just TyBool) s =
  let
    child = genWellTypedTerm' Nothing (s `div` 2)
    childB = genWellTypedTerm' (Just TyBool) (s `div` 2)
    childN = genWellTypedTerm' (Just TyNat) (s `div` 2)
    nonZeroSize = [
        genTmIsZero childB
      , genTmIf childN child child
      , genTmIf childB childN childB
      , genTmIf childB childB childN
      ]
  in
    oneof nonZeroSize
genIllTypedTerm' (Just TyNat) s =
  let
    child = genWellTypedTerm' Nothing (s `div` 2)
    childB = genWellTypedTerm' (Just TyBool) (s `div` 2)
    childN = genWellTypedTerm' (Just TyNat) (s `div` 2)
    nonZeroSize = [
        genTmSucc childB
      , genTmPred childB
      , genTmIf childN child child
      , genTmIf childB childN childB
      , genTmIf childB childB childN
      ]
  in
    oneof nonZeroSize


shrTmZero :: Term l -> Maybe [Term l]
shrTmZero = fmap (const []) . preview _TmZero

shrTmSucc :: (Term l -> [Term l]) -> Term l -> Maybe [Term l]
shrTmSucc shr = fmap shrTmSucc' . preview _TmSucc
  where
    shrTmSucc' t =
      shr t ++
      fmap (review _TmSucc) (shr t)

shrTmPred :: (Term l -> [Term l]) -> Term l -> Maybe [Term l]
shrTmPred shr = fmap shrTmPred' . preview _TmPred
  where
    shrTmPred' t =
      shr t ++
      fmap (review _TmPred) (shr t)

shrTmFalse :: Term l -> Maybe [Term l]
shrTmFalse = fmap (const []) . preview _TmFalse

shrTmTrue :: Term l -> Maybe [Term l]
shrTmTrue = fmap (const []) . preview _TmTrue

shrTmIf :: (Term l -> [Term l]) -> (Term l -> [Term l]) -> (Term l -> [Term l]) -> Term l -> Maybe [Term l]
shrTmIf s1 s2 s3 = fmap shrTmIf' . preview _TmIf
  where
    shrTmIf' (t1, t2, t3) =
      s2 t2 ++
      s3 t3 ++
      fmap (\u1 -> review _TmIf (u1, t2, t3)) (s1 t1) ++
      fmap (\u2 -> review _TmIf (t1, u2, t3)) (s2 t2) ++
      fmap (\u3 -> review _TmIf (t1, t2, u3)) (s3 t3)

shrTmIsZero :: (Term l -> [Term l]) -> Term l -> Maybe [Term l]
shrTmIsZero shr = fmap shrTmIsZero' . preview _TmIsZero
  where
    shrTmIsZero' t =
      fmap (review _TmIsZero) (shr t)

shrinkTerm :: Term l -> [Term l]
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
newtype AnyTerm = AnyTerm { getAnyTerm :: Term Span }
                deriving (Show)

instance Eq AnyTerm where
  AnyTerm x == AnyTerm y =
    stripLoc x == stripLoc y

instance Ord AnyTerm where
  compare (AnyTerm x) (AnyTerm y) =
     compare (stripLoc x) (stripLoc y)

instance Arbitrary AnyTerm where
  arbitrary = AnyTerm <$> genAnyTerm
  shrink = fmap AnyTerm . shrinkTerm . getAnyTerm

newtype WellTypedTerm = WellTypedTerm { getWellTypedTerm :: Term Span }
                      deriving (Show)

instance Eq WellTypedTerm where
  WellTypedTerm x == WellTypedTerm y =
    stripLoc x == stripLoc y

instance Ord WellTypedTerm where
  compare (WellTypedTerm x) (WellTypedTerm y) =
    compare (stripLoc x) (stripLoc y)

instance Arbitrary WellTypedTerm where
  arbitrary = WellTypedTerm <$> genWellTypedTerm Nothing
  shrink = fmap WellTypedTerm . shrinkTerm . getWellTypedTerm

newtype IllTypedTerm = IllTypedTerm { getIllTypedTerm :: Term Span }
                      deriving (Show)

instance Eq IllTypedTerm where
  IllTypedTerm x == IllTypedTerm y =
    stripLoc x == stripLoc y

instance Ord IllTypedTerm where
  compare (IllTypedTerm x) (IllTypedTerm y) =
    compare (stripLoc x) (stripLoc y)

instance Arbitrary IllTypedTerm where
  arbitrary = IllTypedTerm <$> genIllTypedTerm Nothing
  shrink = fmap IllTypedTerm . shrinkTerm . getIllTypedTerm
