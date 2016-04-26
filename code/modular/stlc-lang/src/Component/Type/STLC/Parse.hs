{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Type.STLC.Parse (
    parseTypeInput
  ) where

import Control.Lens (review, view)
import Text.Parser.Combinators ((<?>))
import Text.Parser.Expression (Assoc(..))
import Text.Trifecta.Parser (Parser)

import Common.Parse (reserveOperators, ParserHelperOutput, HasParserHelperOutput(..))
import Common.Text (ExpressionInfo(..))
import Component.Type.Parse (ParseTypeRule(..), ParseTypeInput(..))

import Component.Type.STLC (AsSTLCType(..), WithSTLCType)

parseTyArr :: WithSTLCType ty nTy
           => ParserHelperOutput
           -> Parser (ty nTy -> ty nTy -> ty nTy) -- ^
parseTyArr h =
  let
    ro = view reservedOperator h
  in
    curry (review _TyArr) <$
      ro "->"
      <?> "->"

parseTypeInput :: WithSTLCType ty nTy
               => ParseTypeInput ty nTy
parseTypeInput =
  ParseTypeInput
    [ ParseTypeExpression
       (reserveOperators ["->"])
        (ExpressionInfo AssocRight 6)
        parseTyArr
    ]
