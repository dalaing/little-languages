{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Components.Term.Bool.Pretty (
    prettyTermInput
  ) where

import           Control.Lens                 (preview)
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>), (</>))

import           Common.Pretty                (reservedConstructor,
                                               reservedIdentifier)
import           Component.Term.Pretty        (PrettyTermInput(..), PrettyTermRule (..))

import           Components.Term.Bool         (AsBoolTerm (..), WithBoolTerm)

-- |
prettyTmFalse :: WithBoolTerm tm a
              => tm a       -- ^
              -> Maybe Doc -- ^
prettyTmFalse =
  fmap (const $ reservedConstructor "False") .
  preview _TmFalse

-- |
prettyTmTrue :: WithBoolTerm tm a
             => tm a       -- ^
             -> Maybe Doc -- ^
prettyTmTrue =
  fmap (const $ reservedConstructor "True") .
  preview _TmTrue

-- |
prettyTmIf :: WithBoolTerm tm a
           => (tm a -> Doc) -- ^
           -> tm a         -- ^
           -> Maybe Doc   -- ^
prettyTmIf prettyTerm =
    fmap prettyTmIf' .
    preview _TmIf
  where
    prettyTmIf' (tm1, tm2, tm3) =
      reservedIdentifier "if" <+> prettyTerm tm1 </>
      reservedIdentifier "then" <+> prettyTerm tm2 </>
      reservedIdentifier "else" <+> prettyTerm tm3

-- |
prettyTermInput :: WithBoolTerm tm a
                => PrettyTermInput (tm a)
prettyTermInput =
  PrettyTermInput
    [ PrettyTermBase prettyTmFalse
    , PrettyTermBase prettyTmTrue
    , PrettyTermRecurse prettyTmIf
    ]
