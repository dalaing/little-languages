{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Constraint ((\\), (:-))

import           Common.Pretty                      (prettyToString)
import           Component.Type.Error.Unexpected.Class (AsUnexpected (..))
import           Component                          (ComponentInput (..))
import           Component.Type.Error               (TypeErrorInput (..))
import           Component.Type.Error.Pretty        (PrettyTypeErrorInput (..),
                                                     PrettyTypeErrorRule (..))
import           Component.Type.Note                (AsNoteType (..),
                                                     WithNoteType)
import Extras (Eq1(..))

data Unexpected ty n a =
  Unexpected {
    actual   :: ty n
  , expected :: ty n
  }
  deriving (Eq, Ord, Show)

instance AsUnexpected (Unexpected ty) ty where
  _Unexpected = prism (uncurry Unexpected) $ \u ->
    case u of
      Unexpected ty1 ty2 -> Right (ty1, ty2)

mkExpect :: forall e ty n m. (
              Eq1 ty
            , AsUnexpected e ty
            , MonadError (e n String) m
            )
         => (Eq n => (ty n -> ty n)
         -> ty n
         -> ty n
         -> m ())
mkExpect stripNote ac ex =
  unless ((stripNote ac == stripNote ex) \\ (spanEq1 :: Eq n :- Eq (ty n))) $
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

prettyUnexpected :: AsUnexpected e ty
                 => (ty n -> Doc)
                 -> e n String
                 -> Maybe Doc
prettyUnexpected prettyType =
  fmap (prettyUnexpected' prettyType) .
  preview _Unexpected

prettyUnexpectedSrcLoc :: ( AsUnexpected e ty
                          , WithNoteType ty
                          , Renderable n
                          )
                       => (ty n -> Doc)
                       -> e n String
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
                => ComponentInput r e ty tm
unexpectedInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithType prettyUnexpected]))
    mempty

unexpectedSrcLocInput :: forall r e ty tm. (
                           AsUnexpected e ty
                         , WithNoteType ty
                         )
                      => ComponentInput r e ty tm
unexpectedSrcLocInput =
  ComponentInput
    mempty
    (TypeErrorInput
       (PrettyTypeErrorInput
         [PrettyTypeErrorWithTypeSrcLoc prettyUnexpectedSrcLoc]))
    mempty
