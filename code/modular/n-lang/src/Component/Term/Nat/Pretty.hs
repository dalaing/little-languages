{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Nat.Pretty (
    prettyTermInput
  ) where

import           Control.Lens                 (preview)
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>))

import           Common.Pretty                (reservedConstructor,
                                               reservedIdentifier)
import           Component.Term.Pretty        (PrettyTermInput(..), PrettyTermRule (..))

import           Component.Term.Nat         (AsNatTerm (..), WithNatTerm)

-- |
prettyTmZero :: WithNatTerm tm
             => tm n a       -- ^
             -> Maybe Doc -- ^
prettyTmZero =
  fmap (const $ reservedConstructor "O") .
  preview _TmZero

-- |
prettyTmSucc :: WithNatTerm tm
             => (tm n a -> Doc) -- ^
             -> tm n a         -- ^
             -> Maybe Doc   -- ^
prettyTmSucc prettyTerm =
    fmap prettyTmSucc' .
    preview _TmSucc
  where
    prettyTmSucc' tm =
      reservedIdentifier "S" <+> prettyTerm tm

-- |
prettyTmPred :: WithNatTerm tm
             => (tm n a -> Doc) -- ^
             -> tm n a         -- ^
             -> Maybe Doc   -- ^
prettyTmPred prettyTerm =
    fmap prettyTmPred' .
    preview _TmPred
  where
    prettyTmPred' tm =
      reservedIdentifier "pred" <+> prettyTerm tm

-- |
prettyTermInput :: WithNatTerm tm
                => PrettyTermInput ty nTy tm nTm a
prettyTermInput =
  PrettyTermInput
    [ PrettyTermBase prettyTmZero
    , PrettyTermRecurse prettyTmSucc
    , PrettyTermRecurse prettyTmPred
    ]
