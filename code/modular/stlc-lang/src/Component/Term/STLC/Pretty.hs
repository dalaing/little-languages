{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Component.Term.STLC.Pretty (
    prettyTermInput
  ) where

import           Control.Lens                 (preview, review)
import           Text.PrettyPrint.ANSI.Leijen (Doc, (<+>))
import Data.Constraint.Forall (ForallT)

import           Common.Text (Assoc(..), ExpressionInfo(..))
import           Common.Pretty                (identifier, reservedOperator, reservedIdentifier)
import           Component.Term.Pretty        (PrettyTermInput(..), PrettyTermRule (..))

import           Component.Term.STLC         (AsSTLCTerm (..), AsSTLCVar(..), WithSTLCTerm, app_)

prettyTmVar :: WithSTLCTerm tm ty
            => tm nTy nTm String
            -> Maybe Doc
prettyTmVar tm = do
  v <- preview _TmVar tm
  return $ identifier v

prettyTmLam :: ( WithSTLCTerm tm ty
               , ForallT Monad tm
               )
            => (ty nTy -> Doc)
            -> (tm nTy nTm String -> Doc)
            -> tm nTy nTm String
            -> Maybe Doc
prettyTmLam prettyType prettyTerm tm = do
  let ri = reservedIdentifier
  (v, ty, s) <- preview _TmLam tm
  return $
    ri "\\" <+>
    identifier v <+>
    ri ":" <+>
    prettyType ty <+>
    ri "." <+>
    prettyTerm (app_ (review _TmVar v) s)


prettyTmApp :: Doc
            -> Doc
            -> Doc
prettyTmApp tm1 tm2 =
  tm1 <+>
  reservedOperator "@" <+>
  tm2

prettyTermInput :: ( WithSTLCTerm tm ty
                   , ForallT Monad tm
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
