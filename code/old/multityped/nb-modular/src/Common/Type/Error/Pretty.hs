{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Common.Type.Error.Pretty (
    PrettyTypeErrorInput(..)
  , HasPrettyTypeErrorInput(..)
  , PrettyTypeErrorOutput(..)
  , HasPrettyTypeErrorOutput(..)
  , mkPrettyTypeError
  , prettyTypeErrorUnexpected
  , prettyTypeErrorExpectedEq
  , prettyTypeErrorUnknownType
  ) where

import Control.Lens.TH (makeClassy)
import Control.Lens (preview)
import Data.Bitraversable (bisequenceA)

import Text.Trifecta.Rendering (Renderable(..))
import Text.Trifecta.Result (Err(..), explain)
import Text.PrettyPrint.ANSI.Leijen
import qualified Text.PrettyPrint.ANSI.Leijen as PP ((<$>))
import qualified Data.Set as S

import Common.Recursion
import Common.Note
import Common.Type.Error
import Common.Type.Pretty (PrettyTypeOutput(..))
import Components.Type.Note.Data

data PrettyTypeErrorInput e ty =
  PrettyTypeErrorInput {
    _prettyTypeErrorSteps :: [(ty -> Doc) -> MaybeStep e Doc]
  }

makeClassy ''PrettyTypeErrorInput

instance Monoid (PrettyTypeErrorInput e ty) where
  mempty = PrettyTypeErrorInput mempty
  mappend (PrettyTypeErrorInput p1) (PrettyTypeErrorInput p2) =
    PrettyTypeErrorInput (mappend p1 p2)

data PrettyTypeErrorOutput e ty =
  PrettyTypeErrorOutput {
    _prettyTypeError :: e -> Doc
  , _prettyTypeErrorString :: e -> String
  }

makeClassy ''PrettyTypeErrorOutput

mkPrettyTypeError :: PrettyTypeOutput ty
                  -> PrettyTypeErrorInput e ty
                  -> PrettyTypeErrorOutput e ty
mkPrettyTypeError (PrettyTypeOutput prettyType _) (PrettyTypeErrorInput ps) =
  let
    prTyErr =
      combineMaybeSteps (text "???") .
      fmap ($ prettyType) $
      ps
  in
    PrettyTypeErrorOutput
      prTyErr
      (docString . prTyErr)

docString :: Doc
          -> String
docString d =
  displayS (renderPretty 0.3 80 (plain d)) ""

prettyTypeErrorUnexpected :: ( AsUnexpected e ty
                             , Show (Without ty)
                             , WithoutNote ty
                             , WithNoteType n ty
                             , Renderable n
                             )
                          => PrettyTypeErrorInput e ty
prettyTypeErrorUnexpected =
    PrettyTypeErrorInput [MSBase . prettyErrorUnexpected]
  where
    prettyErrorUnexpected prettyType =
      fmap (prettyErrorUnexpected' prettyType) .
      preview _TeUnexpected

prettyErrorUnexpected' :: ( Show (Without ty)
                          , WithoutNote ty
                          , WithNoteType n ty
                          , Renderable n
                          )
                       => (ty -> Doc)
                       -> (ty, ty)
                       -> Doc
prettyErrorUnexpected' prettyType (ac, ex) =
    maybe woNote wNote  .
    preview _TyNoted $
    ac
  where
    msg = text "Unexpected type:"
    wNote (n, ac') =
      explain
        (render n)
        (Err (Just (msg <+> prettyType ac')) [] (S.singleton (show . stripNote $ ex)))
    woNote =
      hang 2 (msg PP.<$>
          text "actual:" <+> prettyType ac PP.<$>
          text "expected:" <+> prettyType ex)

prettyTypeErrorExpectedEq :: ( AsExpectedEq e ty
                             , Show (Without ty)
                             , WithoutNote ty
                             , WithNoteType n ty
                             , Renderable n
                             )
                          => PrettyTypeErrorInput e ty
prettyTypeErrorExpectedEq =
    PrettyTypeErrorInput [MSBase . prettyErrorExpectedEq]
  where
    prettyErrorExpectedEq prettyType =
      fmap (prettyErrorExpectedEq' prettyType) .
      preview _TeExpectedEq

prettyErrorExpectedEq' :: ( Show (Without ty)
                          , WithoutNote ty
                          , WithNoteType n ty
                          , Renderable n
                          )
                       => (ty -> Doc)
                       -> (ty, ty)
                       -> Doc
prettyErrorExpectedEq' prettyType (t1, t2) =
    maybe woNote wNote .
    bisequenceA $
    (preview _TyNoted t1, preview _TyNoted t2)
  where
    msg = text "Expected these types to be equal:"
    wNote ((n1, t1'), (n2, t2')) =
      explain (render n1) (Err (Just msg) [text "has type" <+> text (show . stripNote $ t1')] S.empty) PP.<$>
      explain (render n2) (Err (Just msg) [text "has type" <+> text (show . stripNote $ t2')] S.empty)
    woNote =
      hang 2 (msg PP.<$>
        text "type 1:" <+> prettyType t1 PP.<$>
        text "type 2:" <+> prettyType t2)

prettyTypeErrorUnknownType :: ( AsUnknownType e n
                             , Renderable n
                             )
                          => PrettyTypeErrorInput e ty
prettyTypeErrorUnknownType =
    PrettyTypeErrorInput [MSBase . const prettyErrorUnknownType]
  where
    prettyErrorUnknownType =
      fmap prettyErrorUnknownType' .
      preview _TeUnknownType

prettyErrorUnknownType' :: Renderable n
                        =>  Maybe n
                        -> Doc
prettyErrorUnknownType' =
    maybe woNote wNote
  where
    msg = text "Unknown type"
    wNote n =
      explain
        (render n)
        (Err (Just msg) [] S.empty)
    woNote =
      msg
