{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Type.STLC.Strip (
    stripNoteTypeInput
  ) where

import Control.Lens (preview, review)

import Component.Type.Parent (AsParentType(..))
import Component.Type.Note.Strip (StripNoteTypeRule(..), StripNoteTypeInput(..))

import Component.Type.STLC (STLCType(..))

stripNoteTyStlc :: forall ty n m. AsParentType (STLCType ty) ty
                => (ty n -> ty m)
                -> ty n
                -> Maybe (ty m)
stripNoteTyStlc r =
    fmap stripNoteTySTLC' .
    preview _ParentType
  where
    stripNoteTySTLC' :: STLCType ty n -> ty m
    stripNoteTySTLC' (TyArr ty1 ty2) =
      review _ParentType (TyArr (r ty1) (r ty2))

stripNoteTypeInput :: AsParentType (STLCType ty) ty => StripNoteTypeInput ty
stripNoteTypeInput =
  StripNoteTypeInput
    [StripNoteTypeRecurse stripNoteTyStlc]
