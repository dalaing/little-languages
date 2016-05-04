{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.NatBool.Pretty (
    prettyTermInput
  ) where

import           Control.Lens                 (preview)
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>))

import           Common.Pretty                (reservedIdentifier)
import           Component.Term.Pretty        (PrettyTermInput(..), PrettyTermRule (..))

import           Component.Term.NatBool         (AsNatBoolTerm (..), WithNatBoolTerm)

-- |
prettyTmIsZero :: WithNatBoolTerm tm
               => (tm nTy nTm a -> Doc) -- ^
               -> tm nTy nTm a         -- ^
               -> Maybe Doc   -- ^
prettyTmIsZero prettyTerm =
    fmap prettyTmIsZero' .
    preview _TmIsZero
  where
    prettyTmIsZero' tm =
      reservedIdentifier "isZero" <+> prettyTerm tm

-- |
prettyTermInput :: WithNatBoolTerm tm
                => PrettyTermInput ty nTy tm nTm a
prettyTermInput =
  PrettyTermInput
    [PrettyTermRecurse prettyTmIsZero]
