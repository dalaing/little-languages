module Common.Gen (
    GenInput(..)
  , GenOutput(..)
  , mkGen
  ) where

import Common.Gen.Type
import Common.Gen.Term

data GenInput ty tm =
  GenInput {
    _genTypeInput :: GenTypeInput ty
  , _genTermInput :: GenTermInput ty tm
  }

instance Monoid (GenInput ty tm) where
  mempty =
    GenInput mempty mempty
  mappend (GenInput ty1 tm1) (GenInput ty2 tm2) =
    GenInput (mappend ty1 ty2) (mappend tm1 tm2)

data GenOutput ty tm =
  GenOutput {
    _genTypeOutput :: GenTypeOutput ty
  , _genTermOutput :: GenTermOutput ty tm
  }

mkGen :: GenInput ty tm
      -> GenOutput ty tm
mkGen (GenInput tyi tmi) =
  let
    tyo = mkGenType tyi
    tmo = mkGenTerm tyo tmi
  in
    GenOutput tyo tmo
