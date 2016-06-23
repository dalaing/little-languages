module Components.Type.Note.Gen where

import Control.Lens (preview)

import Test.QuickCheck (Gen)

import Common.Type.Gen

import Components.Type.Note.Data

genNotTyNote :: WithNoteType n ty
             => (ty -> Gen ty)
             -> ty
             -> Maybe (Gen ty)
genNotTyNote genNotType =
  fmap (genNotType . snd) .
  preview _TyNoted

shrTyNote :: WithNoteType n ty
          => ty
          -> Maybe [ty]
shrTyNote =
  fmap (pure . snd) .
  preview _TyNoted

genTypeInput :: WithNoteType n ty
             => GenTypeInput ty
genTypeInput =
  GenTypeInput
    []
    [NTyRecurse $ const genNotTyNote]
    [ShrTyBase shrTyNote]
