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

-- from 'ansi-wl-pprint'
import           Text.PrettyPrint.ANSI.Leijen (Doc, int, (<+>))

-- from 'parsers'
import           Text.Parser.Expression       (Assoc(..))
import           Text.Parser.Token.Highlight  (Highlight (..))

-- from 'trifecta'
import           Text.Trifecta.Highlight      (withHighlight)

-- local
import           Common.Pretty                (reservedOperator, PrettyRule(..), mkPretty)
import           Common.Text                  (OperatorInfo(..))
import           Term                         (Term (..))

-- $setup
-- >>> import Data.Maybe (fromMaybe)
-- >>> import Text.PrettyPrint.ANSI.Leijen
-- >>> let render r w f d = putStr $ displayS (renderPretty r w (plain (f d))) ""

-- | A pretty printer for 'TmInt'
--
-- >>> render 0.5 40 (fromMaybe (text "???") . prettyTmInt) $ TmInt 3
-- 3
prettyTmInt :: Term
             -> Maybe Doc
prettyTmInt (TmInt i) =
  Just $ withHighlight Number (int i)
prettyTmInt _ =
  Nothing

-- |
matchTmAdd :: Term
           -> Maybe (Term, Term)
matchTmAdd (TmAdd tm1 tm2) =
  Just (tm1, tm2)
matchTmAdd _ =
  Nothing

-- |
prettyTmAdd :: Doc
            -> Doc
            -> Doc
prettyTmAdd d1 d2 =
  d1 <+> reservedOperator "+" <+> d2

-- |
matchTmSub :: Term
           -> Maybe (Term, Term)
matchTmSub (TmSub tm1 tm2) =
  Just (tm1, tm2)
matchTmSub _ =
  Nothing

-- |
prettyTmSub :: Doc
            -> Doc
            -> Doc
prettyTmSub d1 d2 =
  d1 <+> reservedOperator "-" <+> d2

-- |
matchTmMul :: Term
           -> Maybe (Term, Term)
matchTmMul (TmMul tm1 tm2) =
  Just (tm1, tm2)
matchTmMul _ =
  Nothing

-- |
prettyTmMul :: Doc
            -> Doc
            -> Doc
prettyTmMul d1 d2 =
  d1 <+> reservedOperator "*" <+> d2

-- |
matchTmExp :: Term
           -> Maybe (Term, Term)
matchTmExp (TmExp tm1 tm2) =
  Just (tm1, tm2)
matchTmExp _ =
  Nothing

-- |
prettyTmExp :: Doc
            -> Doc
            -> Doc
prettyTmExp d1 d2 =
  d1 <+> reservedOperator "^" <+> d2

-- | The set of pretty printing rules for terms of the I language.
prettyTermRules :: [PrettyRule Term]
prettyTermRules =
  [ PrettyRegular prettyTmInt
  , PrettyOp (OperatorInfo AssocLeft 6) matchTmAdd prettyTmAdd
  , PrettyOp (OperatorInfo AssocLeft 6) matchTmSub prettyTmSub
  , PrettyOp (OperatorInfo AssocLeft 7) matchTmMul prettyTmMul
  , PrettyOp (OperatorInfo AssocRight 8) matchTmExp prettyTmExp
  ]

-- | The pretty printer for terms of the I language.
--
-- This function is built from the contents of 'prettyTermRules'.
-- It will print "???" if none of the rules apply - which should never happen.
--
-- We can print all of the simple forms of the terms:
-- >>> render 0.5 40 prettyTerm (TmInt 3)
-- 3
--
-- >>> render 0.5 40 prettyTerm (TmAdd (TmInt 2) (TmInt 5))
-- 2 + 5
--
-- >>> render 0.5 40 prettyTerm (TmSub (TmInt 2) (TmInt 5))
-- 2 - 5
--
-- >>> render 0.5 40 prettyTerm (TmMul (TmInt 2) (TmInt 5))
-- 2 * 5
--
-- >>> render 0.5 40 prettyTerm (TmExp (TmInt 2) (TmInt 5))
-- 2 ^ 5
--
-- The left associative operators don't need extra brackets when things are grouped to the left:
-- >>> render 0.5 40 prettyTerm (TmAdd (TmAdd (TmInt 3) (TmInt 2)) (TmInt 5))
-- 3 + 2 + 5
--
-- But they do need brackets when the grouping branches to the right:
-- >>> render 0.5 40 prettyTerm (TmAdd (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- 3 + (2 + 5)
--
-- The right associative operators don't need extra brackets when things are grouped to the right:
-- >>> render 0.5 40 prettyTerm (TmExp (TmInt 3) (TmExp (TmInt 2) (TmInt 5)))
-- 3 ^ 2 ^ 5
--
-- But they do need brackets when the grouping branches to the left:
-- >>> render 0.5 40 prettyTerm (TmExp (TmExp (TmInt 3) (TmInt 2)) (TmInt 5))
-- (3 ^ 2) ^ 5
--
-- Multiplication binds more tightly than addition, so we don't want any brackets if we multiply and then add:
-- >>> render 0.5 40 prettyTerm (TmAdd (TmMul (TmInt 3) (TmInt 2)) (TmInt 5))
-- 3 * 2 + 5
--
-- If we are adding and then multiplying, we'll want brackets:
-- >>> render 0.5 40 prettyTerm (TmMul (TmInt 3) (TmAdd (TmInt 2) (TmInt 5)))
-- 3 * (2 + 5)
--
-- This is true regardless of whether the addition is on the left or the right:
-- >>> render 0.5 40 prettyTerm (TmAdd (TmInt 3) (TmMul (TmInt 2) (TmInt 5)))
-- 3 + 2 * 5
--
-- >>> render 0.5 40 prettyTerm (TmMul (TmAdd (TmInt 3) (TmInt 2)) (TmInt 5))
-- (3 + 2) * 5
--
-- Exponentiation binds more tightly than multiplication, so we don't want any brackets if exponentiate and then multiply:
-- >>> render 0.5 40 prettyTerm (TmMul (TmExp (TmInt 3) (TmInt 2)) (TmInt 5))
-- 3 ^ 2 * 5
--
-- If we're multiplying and then exponentiating, we'll want brackets:
-- >>> render 0.5 40 prettyTerm (TmExp (TmInt 3) (TmMul (TmInt 2) (TmInt 5)))
-- 3 ^ (2 * 5)
prettyTerm :: Term
           -> Doc
prettyTerm =
  mkPretty prettyTermRules
