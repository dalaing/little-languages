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
module Component.Type.Nat (
    NatType(..)
  , AsNatType(..)
  , WithNatType
  ) where

import Control.Lens (review)
import Control.Lens.Prism (Prism', prism)

import Component.Type.Note.Strip (StripNoteType(..))

-- |
data NatType (ty :: * -> *) n =
  TyNat -- ^
  deriving (Eq, Ord, Show)

class AsNatType s ty | s -> ty where
  _NatType :: Prism' (s n) (NatType ty n)
  _TyNat :: Prism' (s n) ()
  _TyNat =
    _NatType .
    prism
      (const TyNat)
      (\x -> case x of TyNat -> Right ())

instance (AsNatType ty ty, StripNoteType ty ty) => StripNoteType (NatType ty) ty where
  mapMaybeNoteType _ TyNat = review _TyNat ()

type WithNatType ty = AsNatType ty ty
