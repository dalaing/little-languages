{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Type.Error.ExpectedEq (
    ExpectedEq(..)
  , AsExpectedEq(..)
  , expectEq
  , expectedEqInput
  , expectedEqSrcLocInput
  ) where

import           Control.Monad                      (unless)

import Data.Bitraversable (bisequenceA)
import Text.Trifecta.Result (Err(..), explain)
import Text.Trifecta.Rendering (Renderable(..))
import qualified Data.Set as S
import           Control.Lens                       (preview)
import           Control.Lens.Prism                 (prism)
import           Control.Monad.Error.Lens           (throwing)
import           Control.Monad.Except               (MonadError)
import           Text.PrettyPrint.ANSI.Leijen       (Doc, hang, text, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen       as PP ((<$>))

import           Common.Type.Error.ExpectedEq.Class (AsExpectedEq (..))
import           Component                          (ComponentInput (..))
import           Component.Type.Error               (TypeErrorInput (..))
import           Component.Type.Error.Pretty        (PrettyTypeErrorInput (..),
                                                     PrettyTypeErrorRule (..))
import Component.Type.Note (AsNoteType(..), WithNoteType)

data ExpectedEq ty =
  ExpectedEq ty ty
  deriving (Eq, Ord, Show)

instance AsExpectedEq (ExpectedEq ty) ty where
  _ExpectedEq = prism (uncurry ExpectedEq) $ \u ->
    case u of
      ExpectedEq ty1 ty2 -> Right (ty1, ty2)

expectEq :: ( Eq ty
            , AsExpectedEq e ty
            , MonadError e m
            )
         => ty
         -> ty
         -> m ()
expectEq ty1 ty2 =
  unless (ty1 == ty2) $
    throwing _ExpectedEq (ty1, ty2)

prettyExpectedEq' :: (ty -> Doc)
                  -> (ty, ty)
                  -> Doc
prettyExpectedEq' prettyType (ty1, ty2) =
  hang 2 (text "Expected these types to be equal:" PP.<$>
          text "type 1:" <+> prettyType ty1 PP.<$>
          text "type 2:" <+> prettyType ty2)

prettyExpectedEqSrcLoc' :: ( Show ty
                           , Renderable n
                           )
                        => ((n, ty), (n, ty))
                        -> Doc
prettyExpectedEqSrcLoc' ((n1, ty1), (n2, ty2)) =
    explain (render n1) (Err (Just msg) [text "has type" <+> text (show ty1)] S.empty) PP.<$>
    explain (render n2) (Err (Just msg) [text "has type" <+> text (show ty2)] S.empty)
  where
    msg = text "Expected these types to be equal:"

prettyExpectedEq :: AsExpectedEq e ty
                 => (ty -> Doc)
                 -> e
                 -> Maybe Doc
prettyExpectedEq prettyType =
  fmap (prettyExpectedEq' prettyType) .
  preview _ExpectedEq

expectedEqInput :: AsExpectedEq e ty
                => ComponentInput e ty tm a
expectedEqInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithType prettyExpectedEq]))
    mempty

prettyExpectedEqSrcLoc :: ( Show ty
                          , AsExpectedEq e ty
                          , WithNoteType n ty
                          , Renderable n
                          )
                       => (ty -> Doc)
                       -> e
                       -> Maybe Doc
prettyExpectedEqSrcLoc prettyType =
    let
      prettyExpectedEq'' (t1, t2) =
        maybe (prettyExpectedEq' prettyType (t1, t2)) prettyExpectedEqSrcLoc' .
        bisequenceA $
       (preview _TyNote t1, preview _TyNote t2)
    in
      fmap prettyExpectedEq'' .
      preview _ExpectedEq

expectedEqSrcLocInput :: ( Show ty
                         , AsExpectedEq e ty
                         , WithNoteType n ty
                         , Renderable n
                         )
                      => ComponentInput e ty tm a
expectedEqSrcLocInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithType prettyExpectedEqSrcLoc]))
    mempty

