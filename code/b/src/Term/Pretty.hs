{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Pretty printers for terms of the B language.
-}
{-# LANGUAGE FlexibleContexts #-}
module Term.Pretty (
    prettyTermRules
  , prettyTerm
  ) where

import           Data.Foldable                (asum)
import           Data.Maybe                   (fromMaybe)

import           Common.Pretty                (reservedConstructor,
                                               reservedIdentifier)
import           Text.PrettyPrint.ANSI.Leijen (Doc, text, (<+>), (</>))

import           Term                         (Term (..))

-- |
prettyTmFalse :: Term      -- ^
              -> Maybe Doc -- ^
prettyTmFalse TmFalse =
  Just $ reservedConstructor "False"
prettyTmFalse _ =
  Nothing

-- |
prettyTmTrue :: Term      -- ^
             -> Maybe Doc -- ^
prettyTmTrue TmTrue =
  Just $ reservedConstructor "True"
prettyTmTrue _ =
  Nothing

-- |
prettyTmIf :: (Term -> Doc) -- ^
           -> Term          -- ^
           -> Maybe Doc     -- ^
prettyTmIf prettyTerm (TmIf tm1 tm2 tm3) =
  Just $
    reservedIdentifier "if" <+> prettyTerm tm1 </>
    reservedIdentifier "then" <+> prettyTerm tm2 </>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIf _ _ =
  Nothing

-- |
prettyTermRules :: [Term -> Maybe Doc]
prettyTermRules =
  [ prettyTmFalse
  , prettyTmTrue
  , prettyTmIf prettyTerm
  ]

-- |
prettyTerm :: Term -- ^
           -> Doc  -- ^
prettyTerm tm =
  fromMaybe (text "???") .
  asum .
  fmap ($ tm) $
  prettyTermRules
