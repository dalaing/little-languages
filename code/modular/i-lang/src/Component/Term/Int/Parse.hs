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
parseTmInt :: WithIntTerm tm n a
           => ParserHelperOutput
           -> Parser (tm n a)           -- ^
parseTmInt _ =
  (review _TmIntLit . fromInteger) <$> integer

-- |
parseTmAdd :: WithIntTerm tm n a
           => ParserHelperOutput
           -> Parser (tm n a -> tm n a -> tm n a) -- ^
parseTmAdd h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmAdd) <$
      ro "+"
      <?> "+"

-- |
parseTmSub :: WithIntTerm tm n a
           => ParserHelperOutput
           -> Parser (tm n a -> tm n a -> tm n a) -- ^
parseTmSub h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmSub) <$
      ro "-"
      <?> "-"

-- |
parseTmMul :: WithIntTerm tm n a
           => ParserHelperOutput
           -> Parser (tm n a -> tm n a -> tm n a) -- ^
parseTmMul h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmMul) <$
      ro "*"
      <?> "*"

-- |
parseTmExp :: WithIntTerm tm n a
           => ParserHelperOutput
           -> Parser (tm n a -> tm n a -> tm n a) -- ^
parseTmExp h =
  let
    ro = view reservedOperator h
  in
    curry (review _TmExp) <$
      ro "^"
      <?> "^"

parseTermInput :: WithIntTerm tm nTm a
               => ParseTermInput ty nTy tm nTm a
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
