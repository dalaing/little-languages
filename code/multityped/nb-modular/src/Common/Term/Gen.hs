{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Term.Gen (
    MkAnyTerm(..)
  , MkWellTypedTerm(..)
  , MkIllTypedTerm(..)
  , MkShrinkTerm(..)
  , GenTermInput(..)
  , HasGenTermInput(..)
  , GenTermOutput(..)
  , HasGenTermOutput(..)
  , mkGenTerm
  ) where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Foldable (asum)

import Control.Lens.TH (makeClassy)

import Test.QuickCheck (Gen, sized, oneof)

import Common.Type.Gen (GenTypeOutput(..))

data MkAnyTerm tm =
    ABase (Gen tm)
  | ARecurse (Gen tm -> Gen tm)

atToGen :: Gen tm
        -> Int
        -> MkAnyTerm tm
        -> Maybe (Gen tm)
atToGen _ _ (ABase g)    = Just g
atToGen _ 0 _            = Nothing
atToGen f _ (ARecurse g) = Just $ g f

mkGenAnyTerm :: [MkAnyTerm tm]
             -> Gen tm
mkGenAnyTerm gs =
  sized $ \s ->
    mkGenAnyTerm' gs s

mkGenAnyTerm' :: [MkAnyTerm tm]
              -> Int
              -> Gen tm
mkGenAnyTerm' gs s =
    oneof .
    catMaybes .
    fmap (atToGen child s') $
    gs
  where
    s' = s `div` 2
    child = mkGenAnyTerm' gs s'

data MkWellTypedTerm ty tm =
    WtBase (ty -> Maybe (Gen tm))
  | WtRecurse ((ty -> Gen tm) -> ty -> Maybe (Gen tm))

wtToGen :: (ty -> Gen tm)
        -> ty
        -> Int
        -> MkWellTypedTerm ty tm
        -> Maybe (Gen tm)
wtToGen _ ty _ (WtBase g)    = g ty
wtToGen _ _ 0 _              = Nothing
wtToGen f ty _ (WtRecurse g) = g f ty

mkGenWellTypedTerm :: [MkWellTypedTerm ty tm]
                 -> Gen ty
                 -> Maybe ty
                 -> Gen tm
mkGenWellTypedTerm gs genType Nothing = do
  ty <- genType
  mkGenWellTypedTerm gs genType (Just ty)
mkGenWellTypedTerm gs _ (Just ty) =
  sized $ \s ->
    mkGenWellTypedTerm' gs s ty

mkGenWellTypedTerm' :: [MkWellTypedTerm ty tm]
                    -> Int
                    -> ty
                    -> Gen tm
mkGenWellTypedTerm' gs s ty =
    oneof .
    catMaybes .
    fmap (wtToGen child ty s') $
    gs
  where
    s' = s `div` 2
    child = mkGenWellTypedTerm' gs s'

data MkIllTypedTerm ty tm =
  ItRecurse (Gen ty -> (ty -> Gen ty) -> (ty -> Gen tm) -> ty -> Maybe (Gen tm))

itToGen :: Gen ty
        -> (ty -> Gen ty)
        -> (ty -> Gen tm)
        -> ty
        -> MkIllTypedTerm ty tm
        -> Maybe (Gen tm)
itToGen genType genNotType genTerm ty (ItRecurse g) =
  g genType genNotType genTerm ty

mkGenIllTypedTerm :: [MkWellTypedTerm ty tm]
                  -> [MkIllTypedTerm ty tm]
                  -> Gen ty
                  -> (ty -> Gen ty)
                  -> Maybe ty
                  -> Gen tm
mkGenIllTypedTerm wgs igs genType genNotType Nothing = do
  ty <- genType
  mkGenIllTypedTerm wgs igs genType genNotType (Just ty)
mkGenIllTypedTerm wgs igs genType genNotType (Just ty) =
  sized $ \s ->
    mkGenIllTypedTerm' wgs igs genType genNotType s ty

mkGenIllTypedTerm' :: [MkWellTypedTerm ty tm]
                   -> [MkIllTypedTerm ty tm]
                   -> Gen ty
                   -> (ty -> Gen ty)
                   -> Int
                   -> ty
                   -> Gen tm
mkGenIllTypedTerm' wgs igs genType genNotType s ty =
    oneof . catMaybes $ hs
  where
    s' = s `div` 2
    child = mkGenWellTypedTerm' wgs s'
    hs = fmap (itToGen genType genNotType child ty) igs

data MkShrinkTerm tm =
    ShrTmBase (tm -> Maybe [tm])
  | ShrTmRecurse ((tm -> [tm]) -> tm -> Maybe [tm])

msToShr :: (tm -> [tm])
        -> tm
        -> MkShrinkTerm tm
        -> Maybe [tm]
msToShr _ tm (ShrTmBase g)    = g tm
msToShr f tm (ShrTmRecurse g) = g f tm

mkShrink :: [MkShrinkTerm tm]
         -> tm
         -> [tm]
mkShrink ss =
    shr
  where
    shr tm =
      fromMaybe [] .
      asum .
      fmap (msToShr shr tm) $
      ss

data GenTermInput ty tm =
  GenTermInput {
    _anyGens :: [MkAnyTerm tm]
  , _wellTypedGens :: [MkWellTypedTerm ty tm]
  , _illTypedGens :: [MkIllTypedTerm ty tm]
  , _shrinkTerms :: [MkShrinkTerm tm]
  }

makeClassy ''GenTermInput

-- TODO should this be a semigroup? a lack of rules is probably bad...
instance Monoid (GenTermInput ty tm) where
  mempty =
    GenTermInput mempty mempty mempty mempty
  mappend (GenTermInput a1 w1 i1 s1) (GenTermInput a2 w2 i2 s2) =
    GenTermInput (mappend a1 a2) (mappend w1 w2) (mappend i1 i2) (mappend s1 s2)

data GenTermOutput ty tm =
  GenTermOutput {
    _genAnyTerm :: Gen tm
  , _genWellTypedTerm :: Maybe ty -> Gen tm
  , _genIllTypedTerm :: Maybe ty -> Gen tm
  , _shrinkTerm :: tm -> [tm]
  }

makeClassy ''GenTermOutput

mkGenTerm :: GenTypeOutput ty
          -> GenTermInput ty tm
          -> GenTermOutput ty tm
mkGenTerm (GenTypeOutput t nt _) (GenTermInput a w i s) =
  GenTermOutput
    (mkGenAnyTerm a)
    (mkGenWellTypedTerm w t)
    (mkGenIllTypedTerm w i t nt)
    (mkShrink s)
