{-# LANGUAGE TemplateHaskell #-}
module Math where

import Control.Lens
import Control.Applicative
import Data.Foldable
import Data.Maybe (fromMaybe)

import qualified Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>))

import qualified Text.Parser.Char as PC
import Text.Parser.Token
import Text.Parser.Token.Highlight
import Text.Parser.Expression
import qualified Data.HashSet as HS

import qualified Text.Trifecta as T
import Text.Trifecta.Delta
import Text.Trifecta.Rendering

data Term =
    TmInt Int
  | TmAdd Term Term
  | TmSub Term Term
  | TmMul Term Term
  | TmExp Term Term
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Term

style :: PC.CharParsing m => IdentifierStyle m
style = IdentifierStyle "math" PC.lower PC.alphaNum res Operator ReservedOperator
  where
    res = HS.fromList ["+", "-", "*", "^"]

reserved :: (Monad m, TokenParsing m) => String -> m ()
reserved = reserve style

parseInt :: (Monad m, TokenParsing m) => m Term
parseInt = (review _TmInt . fromInteger) <$> integer

parseAdd :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseAdd = curry (review _TmAdd) <$ reserved "+"

parseSub :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseSub = curry (review _TmSub) <$ reserved "-"

parseMul :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseMul = curry (review _TmMul) <$ reserved "*"

parseExp :: (Monad m, TokenParsing m) => m (Term -> Term -> Term)
parseExp = curry (review _TmExp) <$ reserved "^"

parseTable :: (Monad m, TokenParsing m) => [[Operator m Term]]
parseTable = [
    [Infix parseAdd AssocLeft, Infix parseSub AssocLeft]
  , [Infix parseMul AssocLeft]
  , [Infix parseExp AssocRight]
  ]

parseExpr :: (Monad m, TokenParsing m) => m Term
parseExpr = buildExpressionParser parseTable parseTerm

parseTerm :: (Monad m, TokenParsing m) => m Term
parseTerm = parens parseExpr <|> parseInt

printInt :: Term -> Maybe PP.Doc
printInt = fmap printInt' . preview _TmInt
  where
    printInt' = PP.integer . toInteger

printAdd :: PP.Doc -> PP.Doc -> PP.Doc
printAdd x y = x PP.<+> PP.text "+" PP.<+> y

printSub :: PP.Doc -> PP.Doc -> PP.Doc
printSub x y = x PP.<+> PP.text "-" PP.<+> y

printMul :: PP.Doc -> PP.Doc -> PP.Doc
printMul x y = x PP.<+> PP.text "*" PP.<+> y

printExp :: PP.Doc -> PP.Doc -> PP.Doc
printExp x y = x PP.<+> PP.text "^" PP.<+> y

data PrOperator =
  PrInfix
    (Term -> Maybe (Term, Term))
    (PP.Doc -> PP.Doc -> PP.Doc)
    Assoc

prOp1 :: PrOperator
      -> (Term -> Maybe PP.Doc)
      -> Term
      -> Maybe PP.Doc
prOp1 o@(PrInfix m p a) base t =
  let
    pl' u = if a == AssocLeft
           then prOp1 o base u
           else empty
    pl u = pl' u <|> base u
    pr' u = if a == AssocRight
           then prOp1 o base u
           else empty
    pr u = pr' u <|> base u
  in do
    (x, y) <- m t
    x' <- pl x
    y' <- pr y
    return $ p x' y'


-- find the element that matches separately from everything else
-- reduce the duplication between this and prOp1
prOp2 :: [PrOperator]
      -> (Term -> Maybe PP.Doc)
      -> Term
      -> Maybe PP.Doc
prOp2 os base =
  let
    prOps2' [] t = Nothing
    prOps2' (o@(PrInfix m p a) : os') t =
      let
        pl' u = if a == AssocLeft
           then prOp2 os base u
           else empty
        pl u = pl' u <|> base u
        pr' u = if a == AssocRight
           then prOp2 os base u
           else empty
        pr u = pr' u <|> base u
        r = do
          (x, y) <- m t
          x' <- pl x
          y' <- pr y
          return $ p x' y'
      in
        case r of
          Nothing -> prOps2' os' t
          Just x -> Just x
  in
    prOps2' os

prOpAdd :: PrOperator
prOpAdd = PrInfix (preview _TmAdd) printAdd AssocLeft

prOpSub :: PrOperator
prOpSub = PrInfix (preview _TmSub) printSub AssocLeft

prOpMul :: PrOperator
prOpMul = PrInfix (preview _TmMul) printMul AssocLeft

prOpExp :: PrOperator
prOpExp = PrInfix (preview _TmExp) printExp AssocRight

buildExpressionPrinter :: [[PrOperator]]
                       -> (Term -> Maybe PP.Doc)
                       -> Term
                       -> Maybe PP.Doc
buildExpressionPrinter ps base = base

printTable :: [[PrOperator]]
printTable = [
    [prOpAdd, prOpSub]
  , [prOpMul]
  , [prOpExp]
  ]

prettyExpr :: Term -> Maybe PP.Doc
prettyExpr t =
  -- buildExpressionPrinter printTable prettyTerm
  -- prOp1 prOpAdd prettyTerm t <|> prettyTerm t
  prOp2 [prOpAdd, prOpSub] prettyTerm t <|> prettyTerm t

prettyTerm :: Term -> Maybe PP.Doc
prettyTerm t = printInt t <|> (fmap PP.parens . prettyExpr $ t)

docString :: PP.Doc -> String
docString d = PP.displayS (PP.renderPretty 0.4 40 (PP.plain d)) ""

-- |
--
-- >>> parseString "1"
-- Right (TmInt 1)
--
-- >>> parseString "1 + 2"
-- Right (TmAdd (TmInt 1) (TmInt 2))
--
-- >>> parseString "1 + 2 + 3"
-- Right (TmAdd (TmAdd (TmInt 1) (TmInt 2)) (TmInt 3))
--
-- >>> parseString "1 + (2 + 3)"
-- Right (TmAdd (TmInt 1) (TmAdd (TmInt 2) (TmInt 3)))
--
-- >>> parseString "(1 + 2) + 3"
-- Right (TmAdd (TmAdd (TmInt 1) (TmInt 2)) (TmInt 3))
--
-- >>> parseString "1 - 2 + 3"
-- Right (TmAdd (TmSub (TmInt 1) (TmInt 2)) (TmInt 3))
--
-- >>> parseString "1 - (2 + 3)"
-- Right (TmSub (TmInt 1) (TmAdd (TmInt 2) (TmInt 3)))
--
-- >>> parseString "(1 - 2) + 3"
-- Right (TmAdd (TmSub (TmInt 1) (TmInt 2)) (TmInt 3))
--
-- >>> parseString "1 * 2 + 3"
-- Right (TmMul (TmInt 1) (TmAdd (TmInt 2) (TmInt 3)))
--
-- >>> parseString "1 * (2 + 3)"
-- Right (TmMul (TmInt 1) (TmAdd (TmInt 2) (TmInt 3)))
--
-- >>> parseString "(1 * 2) + 3"
-- Right (TmAdd (TmMul (TmInt 1) (TmInt 2)) (TmInt 3))
--
-- >>> parseString "1 + 2 * 3"
-- Right (TmMul (TmAdd (TmInt 1) (TmInt 2)) (TmInt 3))
--
-- >>> parseString "1 + (2 * 3)"
-- Right (TmAdd (TmInt 1) (TmMul (TmInt 2) (TmInt 3)))
--
-- >>> parseString "(1 + 2) * 3"
-- Right (TmMul (TmAdd (TmInt 1) (TmInt 2)) (TmInt 3))
--
parseString :: String -> Either String Term
parseString s = case T.parseString parseExpr (Lines 0 0 0 0) s of
  T.Success r -> Right r
  T.Failure d -> Left (docString d)

-- |
--
-- >>> printString (TmInt 1)
-- "1"
--
-- >>> printString (TmAdd (TmInt 1) (TmInt 2))
-- "1 + 2"
--
-- >>> printString (TmAdd (TmAdd (TmInt 1) (TmInt 2)) (TmInt 3))
-- "1 + 2 + 3"
--
-- >>> printString (TmAdd (TmInt 1) (TmAdd (TmInt 2) (TmInt 3)))
-- "1 + (2 + 3)"
--
-- >>> printString (TmAdd (TmSub (TmInt 1) (TmInt 2)) (TmInt 3))
-- "1 - 2 + 3"
--
-- >>> printString (TmSub (TmInt 1) (TmAdd (TmInt 2) (TmInt 3)))
-- "1 - (2 + 3)"
--
-- >>> printString (TmMul (TmInt 1) (TmAdd (TmInt 2) (TmInt 3)))
-- "1 * 2 + 3"
--
-- >>> printString (TmAdd (TmMul (TmInt 1) (TmInt 2)) (TmInt 3))
-- "(1 * 2) + 3"
--
-- printString (TmMul (TmAdd (TmInt 1) (TmInt 2)) (TmInt 3))
-- "1 + 2 * 3"
--
-- printString (TmAdd (TmInt 1) (TmMul (TmInt 2) (TmInt 3)))
-- "1 + (2 * 3)"
--
printString :: Term -> String
printString = docString . fromMaybe PP.empty . prettyExpr

