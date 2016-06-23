{-# LANGUAGE FlexibleContexts #-}
module Components.Type.Bool where

import Common.Type

import Components.Type.Bool.Data
import Components.Type.Bool.Gen
import Components.Type.Bool.Parse
import Components.Type.Bool.Pretty

typeInput :: WithBoolType ty
          => TypeInput e ty
typeInput =
  TypeInput
    genTypeInput
    parseTypeInput
    prettyTypeInput
    mempty
