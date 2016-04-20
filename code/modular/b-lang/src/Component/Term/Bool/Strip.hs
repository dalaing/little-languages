{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Term.Bool.Strip (
    stripNoteTermInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Parent (AsParentTerm(..))
import Component.Term.Note.Strip (StripNoteTermRule(..), StripNoteTermInput(..))

import Component.Term.Bool (BoolTerm(..))

stripNoteTmBool :: forall tm n m a. AsParentTerm (BoolTerm tm) tm
                => (tm n a -> tm m a)
                -> tm n a
                -> Maybe (tm m a)
stripNoteTmBool r =
    fmap stripNoteTmBool' .
    preview _ParentTerm
  where
    stripNoteTmBool' :: BoolTerm tm n a -> tm m a
    stripNoteTmBool' TmFalse =
      review _ParentTerm (TmFalse :: BoolTerm tm m a)
    stripNoteTmBool' TmTrue =
      review _ParentTerm (TmTrue :: BoolTerm tm m a)
    stripNoteTmBool' (TmIf tm1 tm2 tm3) =
      review _ParentTerm (TmIf (r tm1) (r tm2) (r tm3))

stripNoteTermInput :: AsParentTerm (BoolTerm tm) tm
                   => StripNoteTermInput tm
stripNoteTermInput =
  StripNoteTermInput
    [StripNoteTermRecurse stripNoteTmBool]
