{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Int.Parse (
    parseTermInput
  ) where

import Control.Lens (view, review)
import Text.Parser.Combinators ((<?>))
import Text.Parser.Token (integer)
import Text.Trifecta.Parser (Parser)

import Common.Text (Assoc(..), ExpressionInfo(..))
import Common.Parse (reserveOperators, ParserHelperOutput, HasParserHelperOutput(..))
import Component.Term.Parse (ParseTermInput(..), ParseTermRule(..))

import Component.Term.Int (AsIntTerm(..), WithIntTerm)

-- |
parseTmInt :: WithIntTerm tm
           => ParserHelperOutput
           -> Parser (tm nTy nTm a)           -- ^
parseTmInt _ =
  (review _TmIntLit . fromInteger) <$> integer

-- |
parseTmAdd :: WithIntTerm tm
           => ParserHelperOutput
           -> Parser (tm nTy nTm a -> tm nTy nTm a -> tm nTy nTm a) -- ^
parseTmAdd h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmAdd) <$
      ro "+"
      <?> "+"

-- |
parseTmSub :: WithIntTerm tm
           => ParserHelperOutput
           -> Parser (tm nTy nTm a -> tm nTy nTm a -> tm nTy nTm a) -- ^
parseTmSub h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmSub) <$
      ro "-"
      <?> "-"

-- |
parseTmMul :: WithIntTerm tm
           => ParserHelperOutput
           -> Parser (tm nTy nTm a -> tm nTy nTm a -> tm nTy nTm a) -- ^
parseTmMul h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmMul) <$
      ro "*"
      <?> "*"

-- |
parseTmExp :: WithIntTerm tm
           => ParserHelperOutput
           -> Parser (tm nTy nTm a -> tm nTy nTm a -> tm nTy nTm a) -- ^
parseTmExp h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmExp) <$
      ro "^"
      <?> "^"

parseTermInput :: WithIntTerm tm
               => ParseTermInput ty tm
parseTermInput =
  ParseTermInput
    [ ParseTermBase mempty parseTmInt
    , ParseTermExpression
        (reserveOperators ["+"])
        (ExpressionInfo AssocLeft 6)
        parseTmAdd
    , ParseTermExpression
        (reserveOperators ["-"])
        (ExpressionInfo AssocLeft 6)
        parseTmSub
    , ParseTermExpression
        (reserveOperators ["*"])
        (ExpressionInfo AssocLeft 7)
        parseTmMul
    , ParseTermExpression
        (reserveOperators ["^"])
        (ExpressionInfo AssocRight 8)
        parseTmExp
    ]
