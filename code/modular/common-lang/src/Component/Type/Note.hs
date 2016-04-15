{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Component.Type.Note (
    NoteType(..)
  , AsNoteType(..)
  , WithNoteType
  ) where

import           Control.Lens.TH (makeClassyPrisms)

data NoteType n ty =
  TyNote n ty
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeClassyPrisms ''NoteType

type WithNoteType n ty = AsNoteType ty n ty
