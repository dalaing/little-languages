module Term.Gen where

import Control.Lens (preview, review)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe, mapMaybe)

import Text.Trifecta.Rendering (Span)

import Test.QuickCheck (Gen, Arbitrary(..), oneof, sized)

import Loc
import Type
import Type.Gen

import Term

genTmVar :: Gen a
         -> Gen (Term a l a)
genTmVar g =
  TmVar <$> g

genTmLam :: Eq a
         => Gen a
         -> Gen (Type l)
         -> Gen (Term a l a)
         -> Gen (Term a l a)
genTmLam gV gTy gTm =
  fmap (review _lam) ((,,) <$> gV <*> gTy <*> gTm)

genTmApp :: Gen (Term n l a)
         -> Gen (Term n l a)
         -> Gen (Term n l a)
genTmApp g1 g2 =
  TmApp <$> g1 <*> g2

genTmFalse :: Gen (Term n l a)
genTmFalse =
  pure TmFalse

genTmTrue :: Gen (Term n l a)
genTmTrue =
  pure TmTrue

genTmIf :: Gen (Term n l a)
        -> Gen (Term n l a)
        -> Gen (Term n l a)
        -> Gen (Term n l a)
genTmIf g1 g2 g3 =
  TmIf <$> g1 <*> g2 <*> g3

-- genWellTypedTmVar should make use of a context
-- a context would also help with testing warnings for shadowing
-- do we generate (Context l a, Term a l a) pairs?

-- maybe write Infer first?

genWellTypedTmLam :: Eq a
                  => Gen a
                  -> (Type l -> Gen (Term a l a))
                  -> Type l
                  -> Maybe (Gen (Term a l a))
genWellTypedTmLam gV gTm (TyArr x y) =
  -- TODO
  -- gTm y
  -- should probably
  -- genTypedContaining y (TmVar v)
  -- or we should have the option at least so that we can
  -- test warnings for unused vars
  Just $ genTmLam gV (pure x) (gTm y)
genWellTypedTmLam _ _ _ =
  Nothing

genWellTypedTmApp :: (Type l -> Gen (Term n l a))
                  -> Type l
                  -> Maybe (Gen (Term n l a))
genWellTypedTmApp g t = Just $ do
  x <- genType
  genTmApp (g (TyArr x t)) (g x)

genWellTypedTmFalse :: Type l
                    -> Maybe (Gen (Term n l a))
genWellTypedTmFalse TyBool =
  Just genTmFalse
genWellTypedTmFalse _ =
  Nothing

genWellTypedTmTrue :: Type l
                   -> Maybe (Gen (Term n l a))
genWellTypedTmTrue TyBool =
  Just genTmTrue
genWellTypedTmTrue _ =
  Nothing

genWellTypedTmIf :: (Type l -> Gen (Term n l a))
                   -> Type l
                   -> Maybe (Gen (Term n l a))
genWellTypedTmIf g t =
  Just $ genTmIf (g TyBool) (g t) (g t)

genWellTypedTerm :: Maybe (Type l)
                 -> Gen (Type l, Term n l a)
genWellTypedTerm Nothing = do
  ty <- genType
  genWellTypedTerm (Just ty)
genWellTypedTerm (Just ty) =
  sized $ \s ->
    (,) ty <$> genWellTypedTerm' s ty

genWellTypedTerm' :: Int
                  -> Type l
                  -> Gen (Term n l a)
genWellTypedTerm' s t =
  let
    zeroSize =
      mapMaybe ($ t)
      [ genWellTypedTmFalse
      , genWellTypedTmTrue
      ] 
    child = genWellTypedTerm' (s `div` 2)
    nonZeroSize =
      mapMaybe (\x -> x child t)
      [ genWellTypedTmIf
      ]
  in
    oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

  -- unexpected
  -- if not-bool any any
  -- if bool not-t not-t
  -- expectedeq
  -- if bool not-t t
  -- if bool t not-t
genIllTypedTmIf :: (Type l -> Gen (Term n l a))
                   -> Type l
                   -> Maybe (Gen (Term n l a))
genIllTypedTmIf g t =
  Just . oneof . map (\x -> x g t) $
  [ genIllTypedTmIf1
  , genIllTypedTmIf2
  , genIllTypedTmIf3
  ]

  -- unexpected
  -- if not-bool any any
genIllTypedTmIf1 :: (Type l -> Gen (Term n l a))
                   -> Type l
                   -> Gen (Term n l a)
genIllTypedTmIf1 g _ = do
  tb <- genNotType TyBool
  ty <- genType
  genTmIf (g tb) (g ty) (g ty)

  -- expectedeq
  -- if bool not-t t
genIllTypedTmIf2 :: (Type l -> Gen (Term n l a))
                   -> Type l
                   -> Gen (Term n l a)
genIllTypedTmIf2 g t = do
  nt <- genNotType t
  genTmIf (g TyBool) (g nt) (g t)

  -- expectedeq
  -- if bool t not-t
genIllTypedTmIf3 :: (Type l -> Gen (Term n l a))
                   -> Type l
                   -> Gen (Term n l a)
genIllTypedTmIf3 g t = do
  nt <- genNotType t
  genTmIf (g TyBool) (g t) (g nt)

-- what's the best API to marginalize over
-- - output types
-- - type errors

genIllTypedTerm :: Maybe (Type l)
                -> Gen (Type l, Term n l a)
genIllTypedTerm Nothing = do
  ty <- genType
  genIllTypedTerm (Just ty)
genIllTypedTerm (Just ty) =
  sized $ \s ->
    (,) ty <$> genIllTypedTerm' s ty

genIllTypedTerm' :: Int
                 -> Type l
                 -> Gen (Term n l a)
genIllTypedTerm' s t =
  let
    child = genWellTypedTerm' (s `div` 2)
    nonZeroSize =
      mapMaybe (\x -> x child t)
      [ genIllTypedTmIf
      ]
  in
    oneof nonZeroSize

genContainingTmIf :: (Type l -> Gen (Term n l a))
                  -> (Type l -> (Type l, Term n l a) -> Gen (Term n l a))
                  -> Type l
                  -> (Type l, Term n l a)
                  -> Maybe (Gen (Term n l a))
genContainingTmIf g gt tyT p =
  let
    branch = genTmIf (gt TyBool p) (g tyT) (g tyT)
    arm1 = genTmIf (g TyBool) (gt tyT p) (g tyT)
    arm2 = genTmIf (g TyBool) (g tyT) (gt tyT p)
  in
    Just . oneof $ [branch, arm1, arm2]

genTypedTermContaining :: Eq l
                       => Maybe (Type l)
                       -> (Type l, Term n l a)
                       -> Gen (Type l, Term n l a)
genTypedTermContaining Nothing p = do
  ty <- genType
  genTypedTermContaining (Just ty) p
genTypedTermContaining (Just ty) p =
  sized $ \s ->
    (,) ty <$> genTypedTermContaining' s ty p

genTypedTermContaining' :: Eq l 
                        => Int
                        -> Type l
                        -> (Type l, Term n l a)
                        -> Gen (Term n l a)
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
      [ genContainingTmIf other
      ]

genContainingIllTypedTerm :: Eq l
                          => Maybe (Type l)
                          -> Gen (Type l, Term n l a)
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
-- we should break this out, like the pieces of genTerm -- we should probably do the same thing for genWellTypedTerm and genIllTypedTerm
-- do we go the whole way down to specific type errors for illtyped term?

genAnyTerm :: Gen (Term n l a)
genAnyTerm = sized genTerm'
  where
    genTerm' s =
      let
        zeroSize = [genTmFalse, genTmTrue]
        child = genTerm' (s `div` 2)
        nonZeroSize = [genTmIf child child child]
      in
        oneof $ (if s == 0 then [] else nonZeroSize) ++ zeroSize

shrTmFalse :: Term n l a
           -> Maybe [Term n l a]
shrTmFalse =
  fmap (const []) .
  preview _TmFalse

shrTmTrue :: Term n l a
          -> Maybe [Term n l a]
shrTmTrue =
  fmap (const []) .
  preview _TmTrue

shrTmIf :: (Term n l a -> [Term n l a])
        -> (Term n l a -> [Term n l a])
        -> (Term n l a -> [Term n l a])
        -> Term n l a
        -> Maybe [Term n l a]
shrTmIf s1 s2 s3 =
    fmap shrTmIf' .
    preview _TmIf
  where
    shrTmIf' (t1, t2, t3) =
      s2 t2 ++
      s3 t3 ++
      fmap (\u1 -> TmIf u1 t2 t3) (s1 t1) ++
      fmap (\u2 -> TmIf t1 u2 t3) (s2 t2) ++
      fmap (\u3 -> TmIf t1 t2 u3) (s3 t3)

shrinkTerm :: Term n l a
           -> [Term n l a]
shrinkTerm t =
  fromMaybe [] .
  asum .
  map ($ t) $ [
    shrTmFalse
  , shrTmTrue
  , shrTmIf shrinkTerm shrinkTerm shrinkTerm
  ]

-- TODO use pretty printer for the show here
newtype AnyTerm = AnyTerm {
    getAnyTerm :: Term String Span String
  } deriving (Show)

instance Eq AnyTerm where
  AnyTerm x == AnyTerm y =
    stripLoc x == stripLoc y

instance Ord AnyTerm where
  compare (AnyTerm x) (AnyTerm y) =
     compare (stripLoc x) (stripLoc y)

instance Arbitrary AnyTerm where
  arbitrary = AnyTerm <$> genAnyTerm
  shrink = fmap AnyTerm . shrinkTerm . getAnyTerm

newtype WellTypedTerm = WellTypedTerm {
    getWellTypedTerm :: Term String Span String
  } deriving (Show)

instance Eq WellTypedTerm where
  WellTypedTerm x == WellTypedTerm y =
    stripLoc x == stripLoc y

instance Ord WellTypedTerm where
  compare (WellTypedTerm x) (WellTypedTerm y) =
    compare (stripLoc x) (stripLoc y)

instance Arbitrary WellTypedTerm where
  arbitrary = (WellTypedTerm . snd) <$> genWellTypedTerm Nothing
  shrink = fmap WellTypedTerm . shrinkTerm . getWellTypedTerm

newtype IllTypedTerm = IllTypedTerm {
    getIllTypedTerm :: Term String Span String
  } deriving (Show)

instance Eq IllTypedTerm where
  IllTypedTerm x == IllTypedTerm y =
    stripLoc x == stripLoc y

instance Ord IllTypedTerm where
  compare (IllTypedTerm x) (IllTypedTerm y) =
    compare (stripLoc x) (stripLoc y)

instance Arbitrary IllTypedTerm where
  arbitrary = (IllTypedTerm . snd) <$> genIllTypedTerm Nothing
  shrink = fmap IllTypedTerm . shrinkTerm . getIllTypedTerm
