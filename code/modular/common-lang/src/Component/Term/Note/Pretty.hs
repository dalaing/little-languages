{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Component.Term.Note.Pretty (
    prettyTermInput
  ) where

import           Control.Lens                 (preview)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Component.Term.Note          (AsNoteTerm (..), WithNoteTerm)
import           Component.Term.Pretty        (PrettyTermInput (..),
                                               PrettyTermRule (..))

prettyTmNote :: WithNoteTerm tm n a
             => (tm n a -> Doc)
             -> tm n a
             -> Maybe Doc
prettyTmNote prettyTerm =
  fmap (prettyTerm . snd) .
  preview _TmNote

prettyTermInput :: WithNoteTerm tm nTm a
                => PrettyTermInput ty nTy tm nTm a
prettyTermInput =
  PrettyTermInput
    [PrettyTermRecurse prettyTmNote]
