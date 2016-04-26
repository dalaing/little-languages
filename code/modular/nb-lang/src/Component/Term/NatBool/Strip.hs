{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Term.NatBool.Strip (
    stripNoteTermInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Parent (AsParentTerm(..))
import Component.Term.Note.Strip (StripNoteTermRule(..), StripNoteTermInput(..))

import Component.Term.NatBool (NatBoolTerm(..))

stripNoteTmIsZero :: forall tm n m a. AsParentTerm (NatBoolTerm tm) tm
                  => (tm n a -> tm m a)
                  -> tm n a
                  -> Maybe (tm m a)
stripNoteTmIsZero r =
    fmap stripNoteTmIsZero' .
    preview _ParentTerm
  where
    stripNoteTmIsZero' :: NatBoolTerm tm n a -> tm m a
    stripNoteTmIsZero' (TmIsZero tm) =
      review _ParentTerm (TmIsZero (r tm))

stripNoteTermInput :: AsParentTerm (NatBoolTerm tm) tm
                   => StripNoteTermInput ty nTy tm
stripNoteTermInput =
  StripNoteTermInput
    [StripNoteTermRecurse stripNoteTmIsZero]
