{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Component.Type.Error.Unexpected (
    Unexpected(..)
  , AsUnexpected(..)
  , mkExpect
  , unexpectedInput
  , unexpectedSrcLocInput
  ) where

import           Control.Monad                      (unless)

import           Control.Lens                       (preview)
import           Control.Lens.Prism                 (prism)
import           Control.Monad.Error.Lens           (throwing)
import           Control.Monad.Except               (MonadError)
import qualified Data.Set                           as S
import           Text.PrettyPrint.ANSI.Leijen       (Doc, hang, text, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen       as PP ((<$>))
import           Text.Trifecta.Rendering            (Renderable (..))
import           Text.Trifecta.Result               (Err (..), explain)

import           Common.Pretty                      (prettyToString)
import           Component.Type.Error.Unexpected.Class (AsUnexpected (..))
import           Component                          (ComponentInput (..))
import           Component.Type.Error               (TypeErrorInput (..))
import           Component.Type.Error.Pretty        (PrettyTypeErrorInput (..),
                                                     PrettyTypeErrorRule (..))
import           Component.Type.Note                (AsNoteType (..),
                                                     WithNoteType)

data Unexpected ty n =
  Unexpected {
    actual   :: ty n
  , expected :: ty n
  }
  deriving (Eq, Ord, Show)

instance AsUnexpected (Unexpected ty n) ty n where
  _Unexpected = prism (uncurry Unexpected) $ \u ->
    case u of
      Unexpected ty1 ty2 -> Right (ty1, ty2)

mkExpect :: ( Eq (ty n)
            , AsUnexpected e ty n
            , MonadError e m
            )
         => (ty n -> ty n)
         -> ty n
         -> ty n
         -> m ()
mkExpect stripNote ac ex =
  unless (stripNote ac == stripNote ex) $
    throwing _Unexpected (ac, ex)

prettyUnexpected' :: (ty n -> Doc)
                  -> (ty n, ty n)
                  -> Doc
prettyUnexpected' prettyType (ty1, ty2) =
  hang 2 (text "Unexpected type:" PP.<$>
          text "actual:" <+> prettyType ty1 PP.<$>
          text "expected:" <+> prettyType ty2)

prettyUnexpectedSrcLoc' :: Renderable n
                        => (ty n -> Doc)
                        -> (n, ty n)
                        -> ty n
                        -> Doc
prettyUnexpectedSrcLoc' prettyType (n, ac) ex =
  explain
    (render n)
      (Err
        (Just (msg <+> prettyType ac))
        []
        (S.singleton (prettyToString . prettyType $ ex)))
  where
    msg = text "Unexpected type:"

prettyUnexpected :: AsUnexpected e ty n
                 => (ty n -> Doc)
                 -> e
                 -> Maybe Doc
prettyUnexpected prettyType =
  fmap (prettyUnexpected' prettyType) .
  preview _Unexpected

prettyUnexpectedSrcLoc :: ( AsUnexpected e ty n
                          , WithNoteType ty n
                          , Renderable n
                          )
                       => (ty n -> Doc)
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

unexpectedInput :: AsUnexpected e ty nTy
                => ComponentInput r e ty nTy tm nTm a
unexpectedInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithType prettyUnexpected]))
    mempty

unexpectedSrcLocInput :: ( AsUnexpected e ty nTy
                         , WithNoteType ty nTy
                         , Renderable nTy
                         )
                      => ComponentInput r e ty nTy tm nTm a
unexpectedSrcLocInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithType prettyUnexpectedSrcLoc]))
    mempty
