{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Type.Bool.Strip (
    stripNoteTypeInput
  ) where

import Control.Lens (preview, review)

import Component.Type.Parent (AsParentType(..))
import Component.Type.Note.Strip (StripNoteTypeRule(..), StripNoteTypeInput(..))

import Component.Type.Bool (BoolType(..))

stripNoteTyBool :: forall ty n m. AsParentType (BoolType ty) ty
                => (ty n -> ty m)
                -> ty n
                -> Maybe (ty m)
stripNoteTyBool _ =
    fmap stripNoteTyBool' .
    preview _ParentType
  where
    stripNoteTyBool' :: BoolType ty n -> ty m
    stripNoteTyBool' _ = review _ParentType (TyBool :: BoolType ty m)

stripNoteTypeInput :: AsParentType (BoolType ty) ty => StripNoteTypeInput ty
stripNoteTypeInput =
  StripNoteTypeInput
    [StripNoteTypeRecurse stripNoteTyBool]
