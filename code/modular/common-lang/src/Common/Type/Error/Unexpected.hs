{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Type.Error.Unexpected (
    Unexpected(..)
  , AsUnexpected(..)
  , expect
  , unexpectedInput
  , unexpectedSrcLocInput
  ) where

import           Control.Monad                      (unless)

import Text.Trifecta.Result (Err(..), explain)
import Text.Trifecta.Rendering (Renderable(..))
import qualified Data.Set as S
import           Control.Lens                       (preview)
import           Control.Lens.Prism                 (prism)
import           Control.Monad.Error.Lens           (throwing)
import           Control.Monad.Except               (MonadError)
import           Text.PrettyPrint.ANSI.Leijen       (Doc, hang, text, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen       as PP ((<$>))

import           Common.Type.Error.Unexpected.Class (AsUnexpected (..))
import           Component                          (ComponentInput (..))
import           Component.Type.Error               (TypeErrorInput (..))
import           Component.Type.Error.Pretty        (PrettyTypeErrorInput (..),
                                                     PrettyTypeErrorRule (..))
import Component.Type.Note (AsNoteType(..), WithNoteType)

data Unexpected ty =
  Unexpected {
    actual   :: ty
  , expected :: ty
  }
  deriving (Eq, Ord, Show)

instance AsUnexpected (Unexpected ty) ty where
  _Unexpected = prism (uncurry Unexpected) $ \u ->
    case u of
      Unexpected ty1 ty2 -> Right (ty1, ty2)

expect :: ( Eq ty
          , AsUnexpected e ty
          , MonadError e m
          )
       => ty
       -> ty
       -> m ()
expect ac ex =
  unless (ac == ex) $
    throwing _Unexpected (ac, ex)

prettyUnexpected' :: (ty -> Doc)
                  -> (ty, ty)
                  -> Doc
prettyUnexpected' prettyType (ty1, ty2) =
  hang 2 (text "Expected type:" PP.<$>
          text "actual:" <+> prettyType ty1 PP.<$>
          text "expected:" <+> prettyType ty2)

prettyUnexpectedSrcLoc' :: ( Show ty
                           , Renderable n
                           )
                        => (ty -> Doc)
                        -> (n, ty)
                        -> ty
                        -> Doc
prettyUnexpectedSrcLoc' prettyType (n, ac) ex =
  explain
    (render n)
      (Err (Just (msg <+> prettyType ac)) [] (S.singleton (show ex)))
  where
    msg = text "Unexpected type:"

prettyUnexpected :: AsUnexpected e ty
                 => (ty -> Doc)
                 -> e
                 -> Maybe Doc
prettyUnexpected prettyType =
  fmap (prettyUnexpected' prettyType) .
  preview _Unexpected

prettyUnexpectedSrcLoc :: ( Show ty
                          , AsUnexpected e ty
                          , WithNoteType n ty
                          , Renderable n
                          )
                       => (ty -> Doc)
                       -> e
                       -> Maybe Doc
prettyUnexpectedSrcLoc prettyType =
    let
      prettyUnexpected'' (ac, ex) =
        maybe (prettyUnexpected' prettyType (ac, ex)) (\ac' -> prettyUnexpectedSrcLoc' prettyType ac' ex) $
        preview _TyNote ac
    in
      fmap prettyUnexpected'' .
      preview _Unexpected

unexpectedInput :: AsUnexpected e ty
                => ComponentInput e ty tm a
unexpectedInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithType prettyUnexpected]))
    mempty

unexpectedSrcLocInput :: ( Show ty
                         , AsUnexpected e ty
                         , WithNoteType n ty
                         , Renderable n
                         )
                      => ComponentInput e ty tm a
unexpectedSrcLocInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithType prettyUnexpectedSrcLoc]))
    mempty
