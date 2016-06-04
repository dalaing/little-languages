{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

Pretty printers for terms of the NB language.
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
import           Text.PrettyPrint.ANSI.Leijen (Doc, group, align, text, (<+>))
import           Text.PrettyPrint.ANSI.Leijen as PP ((<$>))

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
prettyTmSucc :: (Term -> Doc) -- ^ The pretty printer for NB
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
prettyTmPred :: (Term -> Doc) -- ^ The pretty printer for NB
             -> Term
             -> Maybe Doc
prettyTmPred pretty (TmPred tm) =
  Just $ reservedIdentifier "pred" <+> pretty tm
prettyTmPred _ _ =
  Nothing

-- | A pretty printer for 'TmFalse'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTmFalse) $ TmFalse
-- False
prettyTmFalse :: Term
              -> Maybe Doc
prettyTmFalse TmFalse =
  Just $ reservedConstructor "False"
prettyTmFalse _ =
  Nothing

-- | A pretty printer for 'TmTrue'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTmTrue) $ TmTrue
-- True
prettyTmTrue :: Term
             -> Maybe Doc
prettyTmTrue TmTrue =
  Just $ reservedConstructor "True"
prettyTmTrue _ =
  Nothing

-- | A pretty printer for 'TmIf'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . (prettyTmIf prettyTerm)) $ TmIf TmFalse TmFalse TmTrue
-- if False
-- then False
-- else True
prettyTmIf :: (Term -> Doc) -- ^ The pretty printer for NB.
           -> Term
           -> Maybe Doc
prettyTmIf pretty (TmIf tm1 tm2 tm3) =
  Just . group . align $
    reservedIdentifier "if" <+> pretty tm1 PP.<$>
    reservedIdentifier "then" <+> pretty tm2 PP.<$>
    reservedIdentifier "else" <+> pretty tm3
prettyTmIf _ _ =
  Nothing

-- | A pretty printer for 'TmIsZero'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . (prettyTmIsZero prettyTerm)) $ TmIsZero (TmSucc TmZero)
-- isZero S O
prettyTmIsZero :: (Term -> Doc) -- ^ The pretty printer for NB
               -> Term
               -> Maybe Doc
prettyTmIsZero pretty (TmIsZero tm) =
  Just $ reservedIdentifier "isZero" <+> pretty tm
prettyTmIsZero _ _ =
  Nothing

-- | The set of pretty printing rules for terms of the NB language.
prettyTermRules :: [Term -> Maybe Doc]
prettyTermRules =
  [ prettyTmZero
  , prettyTmSucc prettyTerm
  , prettyTmPred prettyTerm
  , prettyTmFalse
  , prettyTmTrue
  , prettyTmIf prettyTerm
  , prettyTmIsZero prettyTerm
  ]

-- | The pretty printer for terms of the NB language.
--
-- This function is built from the contents of 'prettyTermRules'.
-- It will print "???" if none of the rules apply - which should never happen.
--
-- >>> render 0.5 40 prettyTerm (TmSucc (TmSucc (TmPred (TmSucc TmZero))))
-- S S pred S O
--
-- >>> render 0.25 40 prettyTerm (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.75 40 prettyTerm (TmIf TmFalse TmFalse TmTrue)
-- if False then False else True
--
-- >>> render 0.25 40 prettyTerm (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
--    then False
--    else True
-- then if False
--      then False
--      else True
-- else if True
--      then False
--      else True
--
-- >>> render 0.75 40 prettyTerm (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
--    then False
--    else True
-- then if False
--      then False
--      else True
-- else if True
--      then False
--      else True
--
-- >>> render 0.5 40 prettyTerm $ TmIsZero (TmSucc TmZero)
-- isZero S O
prettyTerm :: Term
           -> Doc
prettyTerm tm =
  fromMaybe (text "???") .
  asum .
  fmap ($ tm) $
  prettyTermRules
