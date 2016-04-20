{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Component.Type.Error.ExpectedEq (
    ExpectedEq(..)
  , AsExpectedEq(..)
  , mkExpectEq
  , expectedEqInput
  , expectedEqSrcLocInput
  ) where

import           Control.Monad                      (unless)

import           Control.Lens                       (preview)
import           Control.Lens.Prism                 (prism)
import           Control.Monad.Error.Lens           (throwing)
import           Control.Monad.Except               (MonadError)
import           Data.Bitraversable                 (bisequenceA)
import qualified Data.Set                           as S
import           Text.PrettyPrint.ANSI.Leijen       (Doc, hang, text, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen       as PP ((<$>))
import           Text.Trifecta.Rendering            (Renderable (..))
import           Text.Trifecta.Result               (Err (..), explain)

import           Common.Pretty                      (prettyToString)
import           Component.Type.Error.ExpectedEq.Class (AsExpectedEq (..))
import           Component                          (ComponentInput (..))
import           Component.Type.Error               (TypeErrorInput (..))
import           Component.Type.Error.Pretty        (PrettyTypeErrorInput (..),
                                                     PrettyTypeErrorRule (..))
import           Component.Type.Note                (AsNoteType (..),
                                                     WithNoteType)

data ExpectedEq ty n =
  ExpectedEq (ty n) (ty n)
  deriving (Eq, Ord, Show)

instance AsExpectedEq (ExpectedEq ty n) ty n where
  _ExpectedEq = prism (uncurry ExpectedEq) $ \u ->
    case u of
      ExpectedEq ty1 ty2 -> Right (ty1, ty2)

mkExpectEq :: ( Eq (ty n)
              , AsExpectedEq e ty n
              , MonadError e m
              )
           => (ty n -> ty n)
           -> ty n
           -> ty n
           -> m ()
mkExpectEq stripNote ty1 ty2 =
  unless (stripNote ty1 == stripNote ty2) $
    throwing _ExpectedEq (ty1, ty2)

prettyExpectedEq' :: (ty n -> Doc)
                  -> (ty n, ty n)
                  -> Doc
prettyExpectedEq' prettyType (ty1, ty2) =
  hang 2 (text "Expected these types to be equal:" PP.<$>
          text "type 1:" <+> prettyType ty1 PP.<$>
          text "type 2:" <+> prettyType ty2)

prettyExpectedEqSrcLoc' :: Renderable n
                        => (ty n -> Doc)
                        -> ((n, ty n), (n, ty n))
                        -> Doc
prettyExpectedEqSrcLoc' prettyType ((n1, ty1), (n2, ty2)) =
    explain
      (render n1)
      (Err (Just msg)
       [text "has type" <+> text (prettyToString . prettyType $ ty1)]
       S.empty) PP.<$>
    explain
      (render n2)
        (Err (Just msg)
         [text "has type" <+> text (prettyToString . prettyType $ ty2)]
         S.empty)
  where
    msg = text "Expected these types to be equal:"

prettyExpectedEq :: AsExpectedEq e ty n
                 => (ty n -> Doc)
                 -> e
                 -> Maybe Doc
prettyExpectedEq prettyType =
  fmap (prettyExpectedEq' prettyType) .
  preview _ExpectedEq

expectedEqInput :: AsExpectedEq e ty nTy
                => ComponentInput r e ty nTy tm nTm a
expectedEqInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithType prettyExpectedEq]))
    mempty

prettyExpectedEqSrcLoc :: ( AsExpectedEq e ty n
                          , WithNoteType ty n
                          , Renderable n
                          )
                       => (ty n -> Doc)
                       -> e
                       -> Maybe Doc
prettyExpectedEqSrcLoc prettyType =
    let
      prettyExpectedEq'' (t1, t2) =
        maybe
          (prettyExpectedEq' prettyType (t1, t2))
          (prettyExpectedEqSrcLoc' prettyType) .
        bisequenceA $
       (preview _TyNote t1, preview _TyNote t2)
    in
      fmap prettyExpectedEq'' .
      preview _ExpectedEq

expectedEqSrcLocInput :: ( AsExpectedEq e ty nTy
                         , WithNoteType ty nTy
                         , Renderable nTy
                         )
                      => ComponentInput r e ty nTy tm nTm a
expectedEqSrcLocInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithType prettyExpectedEqSrcLoc]))
    mempty

