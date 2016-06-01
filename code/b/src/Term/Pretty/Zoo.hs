{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable

An internal module used to compare the impact of using the various different kinds of layout rules with the 'ansi-wl-pprint' package.
-}
module Term.Pretty.Zoo (
  ) where

-- from 'base'
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)

-- from 'ansi-wl-pprint'
import Text.PrettyPrint.ANSI.Leijen
import Text.PrettyPrint.ANSI.Leijen as PP ((<$>))

-- local
import Common.Pretty
import Term

-- $setup
-- >>> let render r w f d = putStr $ displayS (renderPretty r w (plain (f d))) ""

prettyTmFalse :: Term
              -> Maybe Doc
prettyTmFalse TmFalse =
  Just $ reservedConstructor "False"
prettyTmFalse _ =
  Nothing

prettyTmTrue :: Term
             -> Maybe Doc
prettyTmTrue TmTrue =
  Just $ reservedConstructor "True"
prettyTmTrue _ =
  Nothing

prettyTmIfS :: (Term -> Doc) -> Term -> Maybe Doc
prettyTmIfS prettyTerm (TmIf tm1 tm2 tm3) =
  Just  $
    reservedIdentifier "if" <+> prettyTerm tm1 </>
    reservedIdentifier "then" <+> prettyTerm tm2 </>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIfS _ _ =
  Nothing

prettyTermRulesS :: [Term -> Maybe Doc]
prettyTermRulesS =
  [prettyTmFalse, prettyTmTrue, prettyTmIfS prettyTermS]

-- |
--
-- >>> render 0.25 40 prettyTermS TmFalse
-- False
--
-- >>> render 0.50 40 prettyTermS TmFalse
-- False
--
-- >>> render 0.75 40 prettyTermS TmFalse
-- False
--
-- >>> render 0.25 40 prettyTermS TmTrue
-- True
--
-- >>> render 0.50 40 prettyTermS TmTrue
-- True
--
-- >>> render 0.75 40 prettyTermS TmTrue
-- True
--
-- >>> render 0.25 40 prettyTermS (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermS (TmIf TmFalse TmFalse TmTrue)
-- if False then False
-- else True
--
-- >>> render 0.75 40 prettyTermS (TmIf TmFalse TmFalse TmTrue)
-- if False then False else True
--
-- >>> render 0.25 40 prettyTermS (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False
-- else True
-- then if False
-- then False
-- else True
-- else if True
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermS (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False else True
-- then if False
-- then False else True
-- else if True
-- then False else True
--
-- >>> render 0.75 40 prettyTermS (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False then False
-- else True then if False
-- then False else True
-- else if True then False
-- else True
--
prettyTermS :: Term -> Doc
prettyTermS tm = fromMaybe (text "???") . asum . fmap ($ tm) $
  prettyTermRulesS

prettyTmIfH :: (Term -> Doc) -> Term -> Maybe Doc
prettyTmIfH prettyTerm (TmIf tm1 tm2 tm3) =
  Just $
    reservedIdentifier "if" <+> prettyTerm tm1 PP.<$>
    reservedIdentifier "then" <+> prettyTerm tm2 PP.<$>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIfH _ _ =
  Nothing

prettyTermRulesH :: [Term -> Maybe Doc]
prettyTermRulesH =
  [prettyTmFalse, prettyTmTrue, prettyTmIfH prettyTermH]

-- |
--
-- >>> render 0.25 40 prettyTermH TmFalse
-- False
--
-- >>> render 0.50 40 prettyTermH TmFalse
-- False
--
-- >>> render 0.75 40 prettyTermH TmFalse
-- False
--
-- >>> render 0.25 40 prettyTermH TmTrue
-- True
--
-- >>> render 0.50 40 prettyTermH TmTrue
-- True
--
-- >>> render 0.75 40 prettyTermH TmTrue
-- True
--
-- >>> render 0.25 40 prettyTermH (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermH (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.75 40 prettyTermH (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.25 40 prettyTermH (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False
-- else True
-- then if False
-- then False
-- else True
-- else if True
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermH (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False
-- else True
-- then if False
-- then False
-- else True
-- else if True
-- then False
-- else True
--
-- >>> render 0.75 40 prettyTermH (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False
-- else True
-- then if False
-- then False
-- else True
-- else if True
-- then False
-- else True
--
prettyTermH :: Term -> Doc
prettyTermH tm = fromMaybe (text "???") . asum . fmap ($ tm) $
  prettyTermRulesH

prettyTmIfSA :: (Term -> Doc) -> Term -> Maybe Doc
prettyTmIfSA prettyTerm (TmIf tm1 tm2 tm3) =
  Just . align $
    reservedIdentifier "if" <+> prettyTerm tm1 </>
    reservedIdentifier "then" <+> prettyTerm tm2 </>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIfSA _ _ =
  Nothing

prettyTermRulesSA :: [Term -> Maybe Doc]
prettyTermRulesSA =
  [prettyTmFalse, prettyTmTrue, prettyTmIfSA prettyTermSA]

-- |
--
-- >>> render 0.25 40 prettyTermSA TmFalse
-- False
--
-- >>> render 0.50 40 prettyTermSA TmFalse
-- False
--
-- >>> render 0.75 40 prettyTermSA TmFalse
-- False
--
-- >>> render 0.25 40 prettyTermSA TmTrue
-- True
--
-- >>> render 0.50 40 prettyTermSA TmTrue
-- True
--
-- >>> render 0.75 40 prettyTermSA TmTrue
-- True
--
-- >>> render 0.25 40 prettyTermSA (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermSA (TmIf TmFalse TmFalse TmTrue)
-- if False then False
-- else True
--
-- >>> render 0.75 40 prettyTermSA (TmIf TmFalse TmFalse TmTrue)
-- if False then False else True
--
-- >>> render 0.25 40 prettyTermSA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
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
-- >>> render 0.50 40 prettyTermSA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
--    then False else True
-- then if False
--      then False else True
-- else if True
--      then False else True
--
-- >>> render 0.75 40 prettyTermSA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False then False
--    else True then if False
--                   then False else True
-- else if True then False
--      else True
--
prettyTermSA :: Term -> Doc
prettyTermSA tm = fromMaybe (text "???") . asum . fmap ($ tm) $
  prettyTermRulesSA

prettyTmIfHA :: (Term -> Doc) -> Term -> Maybe Doc
prettyTmIfHA prettyTerm (TmIf tm1 tm2 tm3) =
  Just . align $
    reservedIdentifier "if" <+> prettyTerm tm1 PP.<$>
    reservedIdentifier "then" <+> prettyTerm tm2 PP.<$>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIfHA _ _ =
  Nothing

prettyTermRulesHA :: [Term -> Maybe Doc]
prettyTermRulesHA =
  [prettyTmFalse, prettyTmTrue, prettyTmIfHA prettyTermHA]

-- |
--
-- >>> render 0.25 40 prettyTermHA TmFalse
-- False
--
-- >>> render 0.50 40 prettyTermHA TmFalse
-- False
--
-- >>> render 0.75 40 prettyTermHA TmFalse
-- False
--
-- >>> render 0.25 40 prettyTermHA TmTrue
-- True
--
-- >>> render 0.50 40 prettyTermHA TmTrue
-- True
--
-- >>> render 0.75 40 prettyTermHA TmTrue
-- True
--
-- >>> render 0.25 40 prettyTermHA (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermHA (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.75 40 prettyTermHA (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.25 40 prettyTermHA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
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
-- >>> render 0.50 40 prettyTermHA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
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
-- >>> render 0.75 40 prettyTermHA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
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
prettyTermHA :: Term -> Doc
prettyTermHA tm = fromMaybe (text "???") . asum . fmap ($ tm) $
  prettyTermRulesHA

prettyTmIfSG :: (Term -> Doc) -> Term -> Maybe Doc
prettyTmIfSG prettyTerm (TmIf tm1 tm2 tm3) =
  Just . group $
    reservedIdentifier "if" <+> prettyTerm tm1 </>
    reservedIdentifier "then" <+> prettyTerm tm2 </>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIfSG _ _ =
  Nothing

prettyTermRulesSG :: [Term -> Maybe Doc]
prettyTermRulesSG =
  [prettyTmFalse, prettyTmTrue, prettyTmIfSG prettyTermSG]

-- |
--
-- >>> render 0.25 40 prettyTermSG TmFalse
-- False
--
-- >>> render 0.50 40 prettyTermSG TmFalse
-- False
--
-- >>> render 0.75 40 prettyTermSG TmFalse
-- False
--
-- >>> render 0.25 40 prettyTermSG TmTrue
-- True
--
-- >>> render 0.50 40 prettyTermSG TmTrue
-- True
--
-- >>> render 0.75 40 prettyTermSG TmTrue
-- True
--
-- >>> render 0.25 40 prettyTermSG (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermSG (TmIf TmFalse TmFalse TmTrue)
-- if False then False
-- else True
--
-- >>> render 0.75 40 prettyTermSG (TmIf TmFalse TmFalse TmTrue)
-- if False then False else True
--
-- >>> render 0.25 40 prettyTermSG (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False
-- else True
-- then if False
-- then False
-- else True
-- else if True
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermSG (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False else True
-- then if False
-- then False else True
-- else if True
-- then False else True
--
-- >>> render 0.75 40 prettyTermSG (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False then False
-- else True then if False
-- then False else True
-- else if True then False
-- else True
--
prettyTermSG :: Term -> Doc
prettyTermSG tm = fromMaybe (text "???") . asum . fmap ($ tm) $
  prettyTermRulesSG

prettyTmIfHG :: (Term -> Doc) -> Term -> Maybe Doc
prettyTmIfHG prettyTerm (TmIf tm1 tm2 tm3) =
  Just . group $
    reservedIdentifier "if" <+> prettyTerm tm1 PP.<$>
    reservedIdentifier "then" <+> prettyTerm tm2 PP.<$>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIfHG _ _ =
  Nothing

prettyTermRulesHG :: [Term -> Maybe Doc]
prettyTermRulesHG =
  [prettyTmFalse, prettyTmTrue, prettyTmIfHG prettyTermHG]

-- |
--
-- >>> render 0.25 40 prettyTermHG TmFalse
-- False
--
-- >>> render 0.50 40 prettyTermHG TmFalse
-- False
--
-- >>> render 0.75 40 prettyTermHG TmFalse
-- False
--
-- >>> render 0.25 40 prettyTermHG TmTrue
-- True
--
-- >>> render 0.50 40 prettyTermHG TmTrue
-- True
--
-- >>> render 0.75 40 prettyTermHG TmTrue
-- True
--
-- >>> render 0.25 40 prettyTermHG (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermHG (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.75 40 prettyTermHG (TmIf TmFalse TmFalse TmTrue)
-- if False then False else True
--
-- >>> render 0.25 40 prettyTermHG (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False
-- else True
-- then if False
-- then False
-- else True
-- else if True
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermHG (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False
-- else True
-- then if False
-- then False
-- else True
-- else if True
-- then False
-- else True
--
-- >>> render 0.75 40 prettyTermHG (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
-- then False
-- else True
-- then if False
-- then False
-- else True
-- else if True
-- then False
-- else True
--
prettyTermHG :: Term -> Doc
prettyTermHG tm = fromMaybe (text "???") . asum . fmap ($ tm) $
  prettyTermRulesHG

prettyTmIfSGA :: (Term -> Doc) -> Term -> Maybe Doc
prettyTmIfSGA prettyTerm (TmIf tm1 tm2 tm3) =
  Just . group . align $
    reservedIdentifier "if" <+> prettyTerm tm1 </>
    reservedIdentifier "then" <+> prettyTerm tm2 </>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIfSGA _ _ =
  Nothing

prettyTermRulesSGA :: [Term -> Maybe Doc]
prettyTermRulesSGA =
  [prettyTmFalse, prettyTmTrue, prettyTmIfSGA prettyTermSGA]

-- |
--
-- >>> render 0.25 40 prettyTermSGA TmFalse
-- False
--
-- >>> render 0.50 40 prettyTermSGA TmFalse
-- False
--
-- >>> render 0.75 40 prettyTermSGA TmFalse
-- False
--
-- >>> render 0.25 40 prettyTermSGA TmTrue
-- True
--
-- >>> render 0.50 40 prettyTermSGA TmTrue
-- True
--
-- >>> render 0.75 40 prettyTermSGA TmTrue
-- True
--
-- >>> render 0.25 40 prettyTermSGA (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermSGA (TmIf TmFalse TmFalse TmTrue)
-- if False then False
-- else True
--
-- >>> render 0.75 40 prettyTermSGA (TmIf TmFalse TmFalse TmTrue)
-- if False then False else True
--
-- >>> render 0.25 40 prettyTermSGA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
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
-- >>> render 0.50 40 prettyTermSGA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False
--    then False else True
-- then if False
--      then False else True
-- else if True
--      then False else True
--
-- >>> render 0.75 40 prettyTermSGA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
-- if if False then False
--    else True then if False
--                   then False else True
-- else if True then False
--      else True
--
prettyTermSGA :: Term -> Doc
prettyTermSGA tm = fromMaybe (text "???") . asum . fmap ($ tm) $
  prettyTermRulesSGA

prettyTmIfHGA :: (Term -> Doc) -> Term -> Maybe Doc
prettyTmIfHGA prettyTerm (TmIf tm1 tm2 tm3) =
  Just . group . align $
    reservedIdentifier "if" <+> prettyTerm tm1 PP.<$>
    reservedIdentifier "then" <+> prettyTerm tm2 PP.<$>
    reservedIdentifier "else" <+> prettyTerm tm3
prettyTmIfHGA _ _ =
  Nothing

prettyTermRulesHGA :: [Term -> Maybe Doc]
prettyTermRulesHGA =
  [prettyTmFalse, prettyTmTrue, prettyTmIfHGA prettyTermHGA]

-- |
--
-- >>> render 0.25 40 prettyTermHGA TmFalse
-- False
--
-- >>> render 0.50 40 prettyTermHGA TmFalse
-- False
--
-- >>> render 0.75 40 prettyTermHGA TmFalse
-- False
--
-- >>> render 0.25 40 prettyTermHGA TmTrue
-- True
--
-- >>> render 0.50 40 prettyTermHGA TmTrue
-- True
--
-- >>> render 0.75 40 prettyTermHGA TmTrue
-- True
--
-- >>> render 0.25 40 prettyTermHGA (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.50 40 prettyTermHGA (TmIf TmFalse TmFalse TmTrue)
-- if False
-- then False
-- else True
--
-- >>> render 0.75 40 prettyTermHGA (TmIf TmFalse TmFalse TmTrue)
-- if False then False else True
--
-- >>> render 0.25 40 prettyTermHGA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
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
-- >>> render 0.50 40 prettyTermHGA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
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
-- >>> render 0.75 40 prettyTermHGA (TmIf (TmIf TmFalse TmFalse TmTrue) (TmIf TmFalse TmFalse TmTrue) (TmIf TmTrue TmFalse TmTrue))
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
prettyTermHGA :: Term -> Doc
prettyTermHGA tm = fromMaybe (text "???") . asum . fmap ($ tm) $
  prettyTermRulesHGA
