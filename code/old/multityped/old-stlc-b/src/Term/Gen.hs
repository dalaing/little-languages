{-# LANGUAGE FlexibleContexts #-}
module Term.Gen where

import Control.Lens (preview, review)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)

import Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

import Type
import Type.Gen

import Term

genTmFalse :: Gen (Term l n a)
genTmFalse = pure (review _TmFalse ())

genTmTrue :: Gen (Term l n a)
genTmTrue = pure (review _TmTrue ())

genTmIf :: Gen (Term l n a) -> Gen (Term l n a) -> Gen (Term l n a) -> Gen (Term l n a)
genTmIf g1 g2 g3 = fmap (review _TmIf) ((,,) <$> g1 <*> g2 <*> g3)

genAnyTerm :: Gen (Term l n a)
genAnyTerm = sized genTerm'
  where
    genTerm' s =
      let
        zeroSize = [genTmFalse, genTmTrue]
        child = genTerm' (s `div` 2)
        nonZeroSize = [genTmIf child child child]
      in
        oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

{-
genWellTypedTerm :: Maybe Type -> Gen (Term l n a)
genWellTypedTerm ty = sized (genWellTypedTerm' ty)

genWellTypedTerm' :: Maybe Type -> Int -> Gen (Term l n a)
genWellTypedTerm' Nothing s = do
  ty <- genType
  genWellTypedTerm' (Just ty) s
genWellTypedTerm' (Just TyBool) s =
  let
    zeroSize = [genTmFalse, genTmTrue]
    childB = genWellTypedTerm' (Just TyBool) (s `div` 2)
    nonZeroSize = [genTmIf childB childB childB]
  in
    oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

genIllTypedTerm :: Maybe Type -> Gen (Term l n a)
genIllTypedTerm ty = sized (genIllTypedTerm' ty)

-- this will produce a term that is ill typed at the outermost constructor
-- use genTermContaining to nest it elsewhere?
genIllTypedTerm' :: Maybe Type -> Int -> Gen (Term l n a)
genIllTypedTerm' Nothing s = do
  ty <- genType
  genIllTypedTerm' (Just ty) s
genIllTypedTerm' (Just TyBool) s =
  let
    child = genWellTypedTerm' Nothing (s `div` 2)
    childB = genWellTypedTerm' (Just TyBool) (s `div` 2)
    nonZeroSize = [
        genTmIf childN child child
      , genTmIf childB childN childB
      , genTmIf childB childB childN
      ]
  in
    oneof nonZeroSize
-}

shrTmFalse :: Term l n a -> Maybe [Term l n a]
shrTmFalse = fmap (const []) . preview _TmFalse

shrTmTrue :: Term l n a-> Maybe [Term l n a]
shrTmTrue = fmap (const []) . preview _TmTrue

shrTmIf :: (Term l n a -> [Term l n a])
        -> (Term l n a -> [Term l n a])
        -> (Term l n a -> [Term l n a])
        -> Term l n a
        -> Maybe [Term l n a]
shrTmIf s1 s2 s3 = fmap shrTmIf' . preview _TmIf
  where
    shrTmIf' (t1, t2, t3) =
      s2 t2 ++
      s3 t3 ++
      fmap (\u1 -> review _TmIf (u1, t2, t3)) (s1 t1) ++
      fmap (\u2 -> review _TmIf (t1, u2, t3)) (s2 t2) ++
      fmap (\u3 -> review _TmIf (t1, t2, u3)) (s3 t3)

-- todo need tmloc?

shrinkTerm :: Term l n a -> [Term l n a]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    shrTmFalse
  , shrTmTrue
  , shrTmIf shrinkTerm shrinkTerm shrinkTerm
  ]

-- TODO use pretty printer for the show here
newtype AnyTerm l n a = AnyTerm { getAnyTerm :: Term l n a }
                deriving (Eq, Ord, Show)

instance Arbitrary (AnyTerm l n a) where
  arbitrary = AnyTerm <$> genAnyTerm
  shrink = fmap AnyTerm . shrinkTerm . getAnyTerm

newtype WellTypedTerm l n a = WellTypedTerm { getWellTypedTerm :: Term l n a }
                      deriving (Eq, Ord, Show)

{-
instance Arbitrary (WellTypedTerm l n a) where
  arbitrary = WellTypedTerm <$> genWellTypedTerm Nothing
  shrink = fmap WellTypedTerm . shrinkTerm . getWellTypedTerm
-}

newtype IllTypedTerm l n a = IllTypedTerm { getIllTypedTerm :: Term l n a}
                      deriving (Eq, Ord, Show)

{-
instance Arbitrary (IllTypedTerm l n a) where
  arbitrary = IllTypedTerm <$> genIllTypedTerm Nothing
  shrink = fmap IllTypedTerm . shrinkTerm . getIllTypedTerm
-}
