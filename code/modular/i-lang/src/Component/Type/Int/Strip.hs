{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Type.Int.Strip (
    stripNoteTypeInput
  ) where

import Control.Lens (preview, review)

import Component.Type.Parent (AsParentType(..))
import Component.Type.Note.Strip (StripNoteTypeRule(..), StripNoteTypeInput(..))

import Component.Type.Int (IntType(..))

stripNoteTyInt :: forall ty n m. AsParentType (IntType ty) ty
                => (ty n -> ty m)
                -> ty n
                -> Maybe (ty m)
stripNoteTyInt _ =
    fmap stripNoteTyInt' .
    preview _ParentType
  where
    stripNoteTyInt' :: IntType ty n -> ty m
    stripNoteTyInt' _ = review _ParentType (TyInt :: IntType ty m)

stripNoteTypeInput :: AsParentType (IntType ty) ty => StripNoteTypeInput ty
stripNoteTypeInput =
  StripNoteTypeInput
    [StripNoteTypeRecurse stripNoteTyInt]
