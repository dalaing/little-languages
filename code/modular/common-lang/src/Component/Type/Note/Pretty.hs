{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Component.Type.Note.Pretty (
    prettyTypeInput
  ) where

import           Control.Lens                 (preview)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Component.Type.Note          (AsNoteType (..), WithNoteType)
import           Component.Type.Pretty        (PrettyTypeInput (..),
                                               PrettyTypeRule (..))

prettyTyNote :: WithNoteType n ty
             => (ty -> Doc)
             -> ty
             -> Maybe Doc
prettyTyNote prettyType =
  fmap (prettyType . snd) .
  preview _TyNote

prettyTypeInput :: WithNoteType n ty
                => PrettyTypeInput ty
prettyTypeInput =
  PrettyTypeInput
    [PrettyTypeRecurse prettyTyNote]
