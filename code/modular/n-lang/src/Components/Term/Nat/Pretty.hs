{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Nat.Pretty (
    prettyTermInput
  ) where

import           Control.Lens                 (preview)
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>))

import           Common.Pretty                (reservedConstructor,
                                               reservedIdentifier)
import           Component.Term.Pretty        (PrettyTermInput(..), PrettyTermRule (..))

import           Components.Term.Nat         (AsNatTerm (..), WithNatTerm)

-- |
prettyTmZero :: WithNatTerm tm a
             => tm a       -- ^
             -> Maybe Doc -- ^
prettyTmZero =
  fmap (const $ reservedConstructor "O") .
  preview _TmZero

-- |
prettyTmSucc :: WithNatTerm tm a
             => (tm a -> Doc) -- ^
             -> tm a         -- ^
             -> Maybe Doc   -- ^
prettyTmSucc prettyTerm =
    fmap prettyTmSucc' .
    preview _TmSucc
  where
    prettyTmSucc' tm =
      reservedIdentifier "S" <+> prettyTerm tm

-- |
prettyTmPred :: WithNatTerm tm a
             => (tm a -> Doc) -- ^
             -> tm a         -- ^
             -> Maybe Doc   -- ^
prettyTmPred prettyTerm =
    fmap prettyTmPred' .
    preview _TmPred
  where
    prettyTmPred' tm =
      reservedIdentifier "pred" <+> prettyTerm tm

-- |
prettyTermInput :: WithNatTerm tm a
                => PrettyTermInput (tm a)
prettyTermInput =
  PrettyTermInput
    [ PrettyTermBase prettyTmZero
    , PrettyTermRecurse prettyTmSucc
    , PrettyTermRecurse prettyTmPred
    ]
