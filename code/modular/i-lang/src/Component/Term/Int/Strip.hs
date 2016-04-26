{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Term.Int.Strip (
    stripNoteTermInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Parent (AsParentTerm(..))
import Component.Term.Note.Strip (StripNoteTermRule(..), StripNoteTermInput(..))

import Component.Term.Int (IntTerm(..))

stripNoteTmInt :: forall tm n m a. AsParentTerm (IntTerm tm) tm
               => (tm n a -> tm m a)
               -> tm n a
               -> Maybe (tm m a)
stripNoteTmInt r =
    fmap stripNoteTmNat' .
    preview _ParentTerm
  where
    stripNoteTmNat' :: IntTerm tm n a -> tm m a
    stripNoteTmNat' (TmIntLit i) =
      review _ParentTerm (TmIntLit i :: IntTerm tm m a)
    stripNoteTmNat' (TmAdd tm1 tm2) =
      review _ParentTerm (TmAdd (r tm1) (r tm2))
    stripNoteTmNat' (TmSub tm1 tm2) =
      review _ParentTerm (TmSub (r tm1) (r tm2))
    stripNoteTmNat' (TmMul tm1 tm2) =
      review _ParentTerm (TmMul (r tm1) (r tm2))
    stripNoteTmNat' (TmExp tm1 tm2) =
      review _ParentTerm (TmExp (r tm1) (r tm2))

stripNoteTermInput :: AsParentTerm (IntTerm tm) tm
                   => StripNoteTermInput ty nTy tm
stripNoteTermInput =
  StripNoteTermInput
    [StripNoteTermRecurse stripNoteTmInt]
