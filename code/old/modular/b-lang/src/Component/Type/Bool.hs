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
{-# LANGUAGE KindSignatures #-}
module Component.Type.Bool (
    BoolType(..)
  , AsBoolType(..)
  , WithBoolType
  ) where

import Control.Lens (review)
import Control.Lens.Prism (Prism', prism)

import Component.Type.Note.Strip (StripNoteType(..))

-- |
data BoolType (ty :: * -> *) n =
  TyBool -- ^
  deriving (Eq, Ord, Show)

class AsBoolType s ty | s -> ty where
  _BoolType :: Prism' (s n) (BoolType ty n)
  _TyBool :: Prism' (s n) ()
  _TyBool =
    _BoolType .
    prism
      (const TyBool)
      (\x -> case x of TyBool -> Right ())

instance (AsBoolType ty ty, StripNoteType ty ty) => StripNoteType (BoolType ty) ty where
  mapMaybeNoteType _ TyBool = review _TyBool ()

type WithBoolType ty = AsBoolType ty ty
