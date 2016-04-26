{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component.Term.Nat.Strip (
    stripNoteTermInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Parent (AsParentTerm(..))
import Component.Term.Note.Strip (StripNoteTermRule(..), StripNoteTermInput(..))

import Component.Term.Nat (NatTerm(..))

stripNoteTmNat :: forall tm n m a. AsParentTerm (NatTerm tm) tm
                => (tm n a -> tm m a)
                -> tm n a
                -> Maybe (tm m a)
stripNoteTmNat r =
    fmap stripNoteTmNat' .
    preview _ParentTerm
  where
    stripNoteTmNat' :: NatTerm tm n a -> tm m a
    stripNoteTmNat' TmZero =
      review _ParentTerm (TmZero :: NatTerm tm m a)
    stripNoteTmNat' (TmSucc tm) =
      review _ParentTerm (TmSucc (r tm))
    stripNoteTmNat' (TmPred tm) =
      review _ParentTerm (TmPred (r tm))

stripNoteTermInput :: AsParentTerm (NatTerm tm) tm
                   => StripNoteTermInput ty nTy tm
stripNoteTermInput =
  StripNoteTermInput
    [StripNoteTermRecurse stripNoteTmNat]
