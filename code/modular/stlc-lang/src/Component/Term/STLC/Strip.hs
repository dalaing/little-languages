{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Component.Term.STLC.Strip (
    stripNoteTermInput
  ) where

import Data.Proxy (Proxy(..))

import Control.Lens (preview, review)
import Bound.Scope (hoistScope)

import Component.Term.Parent (AsParentTerm(..))
import Component.Term.Note.Strip (StripNoteTermRule(..), StripNoteTermInput(..))

import Component.Term.STLC (STLCTerm(..), STLCVar(..))

stripNoteTmVar :: forall tm nTm mTm a. AsParentTerm (STLCVar tm) tm
               => (tm nTm a -> tm mTm a)
               -> tm nTm a
               -> Maybe (tm mTm a)
stripNoteTmVar r =
    fmap stripNoteTmVar' .
    preview _ParentTerm
  where
    stripNoteTmVar' :: STLCVar tm nTm a -> tm mTm a
    stripNoteTmVar' (TmVar x) =
      review _ParentTerm (TmVar x :: STLCVar tm mTm a)

stripNoteTmStlc :: forall ty nTy tm nTm mTm a. (AsParentTerm (STLCTerm ty nTy tm) tm
                   , Functor (tm nTm)
                   )
                => Proxy ty
                -> Proxy nTy
                -> (forall x. tm nTm x -> tm mTm x)
                -> tm nTm a
                -> Maybe (tm mTm a)
stripNoteTmStlc _ _ r =
    fmap stripNoteTmStlc' .
    preview _ParentTerm
  where
    stripNoteTmStlc' :: STLCTerm ty nTy tm nTm a -> tm mTm a
    stripNoteTmStlc' (TmLam n t s) =
      review _ParentTerm (TmLam n t (hoistScope r s))
    stripNoteTmStlc' (TmApp tm1 tm2) =
      review _ParentTerm (TmApp (r tm1) (r tm2))

stripNoteTermInput :: forall ty nTy tm. (AsParentTerm (STLCTerm ty nTy tm) tm
                      , AsParentTerm (STLCVar tm) tm
                      )
                   => StripNoteTermInput ty nTy tm
stripNoteTermInput =
  StripNoteTermInput
    [ StripNoteTermRecurse stripNoteTmVar
    , StripNoteTermRecurse (stripNoteTmStlc (Proxy :: Proxy ty) (Proxy :: Proxy nTy))
    ]
