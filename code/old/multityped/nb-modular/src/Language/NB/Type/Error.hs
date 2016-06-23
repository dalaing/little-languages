{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module Language.NB.Type.Error where

import           Control.Lens.TH          (makeClassyPrisms)
import Text.Trifecta.Rendering (Renderable)

import Components.Type.Note.Data

import Common.Note
import           Common.Type
import           Common.Type.Error
import           Common.Type.Error.Pretty

data Error n ty =
    ErrUnexpected (Unexpected ty)
  | ErrExpectedEq (ExpectedEq ty)
  | ErrUnknownType (UnknownType n)
  deriving (Eq, Ord, Show)

makeClassyPrisms ''Error

instance AsUnexpected (Error n ty) ty where
  _Unexpected = _ErrUnexpected

instance AsExpectedEq (Error n ty) ty where
  _ExpectedEq = _ErrExpectedEq

instance AsUnknownType (Error n ty) n where
  _UnknownType = _ErrUnknownType

errorTypeInput :: ( Show (Without ty)
                  , WithoutNote ty
                  , WithNoteType n ty
                  , Renderable n
                  )
               => TypeInput (Error n ty) ty
errorTypeInput =
  TypeInput
    mempty
    mempty
    mempty
    (mconcat
      [ prettyTypeErrorUnexpected
      , prettyTypeErrorExpectedEq
      , prettyTypeErrorUnknownType
      ]
    )
