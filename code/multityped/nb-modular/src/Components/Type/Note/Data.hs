{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Components.Type.Note.Data where

import Data.Bifunctor

import Control.Lens.TH (makeClassyPrisms)

import Common.Note

data NoteType n f =
  TyNoted n f
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NoteType

type WithNoteType n ty = AsNoteType ty n ty

instance Bifunctor NoteType where
  bimap g h (TyNoted n f) = TyNoted (g n) (h f)

instance WithoutNote f => WithoutNote (NoteType n f) where
  type Without (NoteType n f) = Without f
  stripNote (TyNoted _ f) = stripNote f

