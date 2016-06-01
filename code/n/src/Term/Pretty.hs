{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Pretty printers for terms of the N language.
-}
{-# LANGUAGE FlexibleContexts #-}
module Term.Pretty (
    prettyTermRules
  , prettyTerm
  ) where

-- from 'base'
import           Data.Foldable                (asum)
import           Data.Maybe                   (fromMaybe)

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc, text, (<+>))

-- local
import           Common.Pretty                (reservedConstructor,
                                               reservedIdentifier)
import           Term                         (Term (..))

-- $setup
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let render r w f d = putStr $ displayS (renderPretty r w (plain (f d))) ""

-- | A pretty printer for 'TmZero'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTmZero) $ TmZero
-- O
prettyTmZero :: Term
              -> Maybe Doc
prettyTmZero TmZero =
  Just $ reservedConstructor "O"
prettyTmZero _ =
  Nothing

-- | A pretty printer for 'TmSucc'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . (prettyTmSucc prettyTerm)) $ TmSucc TmZero
-- S O
prettyTmSucc :: (Term -> Doc) -- ^ The pretty printer for N
             -> Term
             -> Maybe Doc
prettyTmSucc pretty (TmSucc tm) =
  Just $ reservedConstructor "S" <+> pretty tm
prettyTmSucc _ _ =
  Nothing

-- | A pretty printer for 'TmPred'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . (prettyTmPred prettyTerm)) $ TmPred TmZero
-- pred O
prettyTmPred :: (Term -> Doc) -- ^ The pretty printer for N
             -> Term
             -> Maybe Doc
prettyTmPred pretty (TmPred tm) =
  Just $ reservedIdentifier "pred" <+> pretty tm
prettyTmPred _ _ =
  Nothing

-- | The set of pretty printing rules for terms of the N language.
prettyTermRules :: [Term -> Maybe Doc]
prettyTermRules =
  [ prettyTmZero
  , prettyTmSucc prettyTerm
  , prettyTmPred prettyTerm
  ]

-- | The pretty printer for terms of the N language.
--
-- This function is built from the contents of 'prettyTermRules'.
-- It will print "???" if none of the rules apply - which should never happen.
--
-- >>> render 0.5 40 prettyTerm (TmSucc (TmSucc (TmPred (TmSucc TmZero))))
-- S S pred S O
prettyTerm :: Term
           -> Doc
prettyTerm tm =
  fromMaybe (text "???") .
  asum .
  fmap ($ tm) $
  prettyTermRules
