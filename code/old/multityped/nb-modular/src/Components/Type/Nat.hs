{-# LANGUAGE FlexibleContexts #-}
module Components.Type.Nat where

import Common.Type

import Components.Type.Nat.Data
import Components.Type.Nat.Gen
import Components.Type.Nat.Parse
import Components.Type.Nat.Pretty

typeInput :: WithNatType ty
          => TypeInput e ty
typeInput =
  TypeInput
    genTypeInput
    parseTypeInput
    prettyTypeInput
    mempty
