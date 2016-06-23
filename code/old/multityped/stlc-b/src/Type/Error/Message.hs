{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Type.Error.Message where

import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import Data.Bitraversable

import Control.Lens (preview)

import qualified Data.Set as S

import Text.Trifecta.Rendering
import Text.Trifecta.Result

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import Loc

import Type
import Type.Pretty
import Type.Error

prettyErrorUnexpected :: Renderable l
                      => TypeError l
                      -> Maybe Doc
prettyErrorUnexpected =
  fmap prettyErrorUnexpected' .
  preview _Unexpected

prettyErrorUnexpected' :: Renderable l
                       => (Type l, Type ())
                       -> Doc
prettyErrorUnexpected' (ac, ex) =
    maybe woLoc wLoc  .
    preview _Loc $
    ac
  where
    msg = text "Unexpected type:"
    wLoc (l, ac') =
      explain
        (render l)
        (Err (Just (msg <+> text (show . stripLoc $ ac'))) [] (S.singleton (show . stripLoc $ ex)))
    woLoc =
      hang 2 (msg PP.<$>
          text "actual:" <+> prettyType ac PP.<$>
          text "expected:" <+> prettyType ex)

prettyErrorExpectedEq :: Renderable l
                      => TypeError l
                      -> Maybe Doc
prettyErrorExpectedEq =
  fmap prettyErrorExpectedEq' .
  preview _ExpectedEq

prettyErrorExpectedEq' :: Renderable l
                      => (Type l, Type l)
                      -> Doc
prettyErrorExpectedEq' (t1, t2) =
    maybe woLoc wLoc .
    bisequence $
    (preview _Loc t1, preview _Loc t2)
  where
    msg = text "Expected these types to be equal:"
    wLoc ((l1, t1'), (l2, t2')) =
      explain (render l1) (Err (Just msg) [text "has type" <+> text (show . stripLoc $ t1')] S.empty) PP.<$>
      explain (render l2) (Err (Just msg) [text "has type" <+> text (show . stripLoc $ t2')] S.empty)
    woLoc =
      hang 2 (msg PP.<$>
        text "type 1:" <+> prettyType t1 PP.<$>
        text "type 2:" <+> prettyType t2)

prettyErrorUnknownType :: Renderable l
                       => TypeError l
                       -> Maybe Doc
prettyErrorUnknownType =
    fmap prettyErrorUnknownType' .
    preview _UnknownType

prettyErrorUnknownType' :: Renderable l
                        => Maybe l
                        -> Doc
prettyErrorUnknownType' =
    maybe woLoc wLoc
  where
    msg = text "Unknown type"
    wLoc l =
      explain
        (render l)
        (Err (Just msg) [] S.empty)
    woLoc =
      msg

-- TODO deal with Nothing locations
prettyError :: Renderable l => TypeError l -> Doc
prettyError t =
  fromMaybe empty .
  asum .
  map ($ t) $ [
    prettyErrorUnexpected
  , prettyErrorExpectedEq
  , prettyErrorUnknownType
  ]
