{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
module Component.Type.Int (
    IntType(..)
  , AsIntType(..)
  , WithIntType
  ) where

import Control.Lens (review)
import Control.Lens.Prism (Prism', prism)

import Component.Type.Note.Strip (StripNoteType(..))

-- |
data IntType (ty :: * -> *) n =
  TyInt -- ^
  deriving (Eq, Ord, Show)

class AsIntType s ty | s -> ty where
  _IntType :: Prism' (s n) (IntType ty n)
  _TyInt :: Prism' (s n) ()
  _TyInt =
    _IntType .
    prism
      (const TyInt)
      (\x -> case x of TyInt -> Right ())

instance (AsIntType ty ty, StripNoteType ty ty) => StripNoteType (IntType ty) ty where
  mapMaybeNoteType _ TyInt = review _TyInt ()

type WithIntType ty = AsIntType ty ty
