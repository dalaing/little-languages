{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Component.Term.STLC (
    STLCVar(..)
  , AsSTLCVar(..)
  , STLCTerm(..)
  , AsSTLCTerm(..)
  , WithSTLCTerm
  , lam_
  ) where

import           Control.Lens       (review)
import           Control.Lens.TH    (makeClassyPrisms)

import           Bound              (Bound (..), Scope (..), abstract1)
import           Bound.Scope        (bitraverseScope)
import           Data.Bifoldable    (Bifoldable (..))
import           Data.Bifunctor     (Bifunctor (..))
import           Data.Bitraversable (Bitraversable (..), bifoldMapDefault,
                                     bimapDefault)

import           Bound2             (Bound2 (..))

-- |
data STLCVar (tm :: * -> * -> *) n a =
    TmVar a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor (STLCVar tm) where
  bimap _ r (TmVar x) = TmVar (r x)

instance Bifoldable (STLCVar tm) where
  bifoldMap _ r (TmVar x) = r x

instance Bitraversable (STLCVar tm) where
  bitraverse _ r (TmVar x) = TmVar <$> r x

makeClassyPrisms ''STLCVar

-- |
data STLCTerm ty nTy tm nTm a =
    TmLam String (ty nTy) (Scope () (tm nTm) a)
  | TmApp (tm nTm a) (tm nTm a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''STLCTerm

instance Bitraversable tm => Bifunctor (STLCTerm ty nTy tm) where
  bimap = bimapDefault

instance Bitraversable tm => Bifoldable (STLCTerm ty nTy tm) where
  bifoldMap = bifoldMapDefault

instance Bitraversable tm => Bitraversable (STLCTerm ty nTy tm) where
  bitraverse l r (TmLam n t s) = TmLam n t <$> bitraverseScope l r s
  bitraverse l r (TmApp tm1 tm2) = TmApp <$> bitraverse l r tm1 <*> bitraverse l r tm2

instance Bound2 (STLCTerm ty nTy) where
  TmLam n ty s  >>>>= f = TmLam n ty (s >>>= f)
  TmApp tm1 tm2 >>>>= f = TmApp (tm1 >>= f) (tm2 >>= f)

type WithSTLCTerm tm ty nTy nTm a =
  ( AsSTLCTerm (tm nTm a) ty nTy tm nTm a
  , AsSTLCVar (tm nTm a) tm nTm a
  )

-- TODO turn into an iso, where we instantiate with TmVar v ?
lam_ :: ( WithSTLCTerm tm ty nTy nTm String
        , Monad (tm nTm)
        )
     => String
     -> ty nTy
     -> tm nTm String
     -> tm nTm String
lam_ v ty tm =
  review _TmLam (v, ty, abstract1 v tm)
