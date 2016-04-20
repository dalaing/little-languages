{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Type.Nat.Strip (
    stripNoteTypeInput
  ) where

import Control.Lens (preview, review)

import Component.Type.Parent (AsParentType(..))
import Component.Type.Note.Strip (StripNoteTypeRule(..), StripNoteTypeInput(..))

import Component.Type.Nat (NatType(..))

stripNoteTyNat :: forall ty n m. AsParentType (NatType ty) ty
                => (ty n -> ty m)
                -> ty n
                -> Maybe (ty m)
stripNoteTyNat _ =
    fmap stripNoteTyNat' .
    preview _ParentType
  where
    stripNoteTyNat' :: NatType ty n -> ty m
    stripNoteTyNat' _ = review _ParentType (TyNat :: NatType ty m)

stripNoteTypeInput :: AsParentType (NatType ty) ty => StripNoteTypeInput ty
stripNoteTypeInput =
  StripNoteTypeInput
    [StripNoteTypeRecurse stripNoteTyNat]
