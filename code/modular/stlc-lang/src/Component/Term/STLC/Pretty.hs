{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Component.Term.STLC.Pretty (
    prettyTermInput
  ) where

import           Control.Lens                 (preview, review)
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>))
import Bound (instantiate1)

import           Common.Text (Assoc(..), ExpressionInfo(..))
import           Common.Pretty                (identifier, reservedOperator, reservedIdentifier)
import           Component.Term.Pretty        (PrettyTermInput(..), PrettyTermRule (..))
import           Component.Type.Pretty        (PrettyTypeOutput(..))

import           Component.Term.STLC         (AsSTLCTerm (..), AsSTLCVar(..), WithSTLCTerm)

prettyTmVar :: WithSTLCTerm tm ty nTy nTm String
            => tm nTm String
            -> Maybe Doc
prettyTmVar tm = do
  v <- preview _TmVar tm
  return $ identifier v

prettyTmLam :: ( WithSTLCTerm tm ty nTy nTm String
               , Monad (tm nTm)
               )
            => (ty nTy -> Doc)
            -> (tm nTm String -> Doc)
            -> tm nTm String
            -> Maybe Doc
prettyTmLam prettyType prettyTerm tm = do
  let ri = reservedIdentifier
  (v, ty, s) <- preview _TmLam tm
  return $
    ri "\\" <+>
    identifier v <+>
    ri ":" <+>
    prettyType ty <+>
    ri "->" <+>
    prettyTerm (instantiate1 (review _TmVar v) s)


prettyTmApp :: Doc
            -> Doc
            -> Doc
prettyTmApp tm1 tm2 =
  tm1 <+>
  reservedOperator "@" <+>
  tm2

prettyTermInput :: ( WithSTLCTerm tm ty nTy nTm String
                   , Monad (tm nTm)
                   )
                => PrettyTermInput ty nTy tm nTm String
prettyTermInput =
  PrettyTermInput
    [ PrettyTermBase prettyTmVar
    , PrettyTermWithType prettyTmLam
    , PrettyTermExpression
      (ExpressionInfo AssocRight 9)
      (preview _TmApp)
      prettyTmApp
    ]
