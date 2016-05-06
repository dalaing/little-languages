{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE FlexibleInstances#-}
module Component.Type.Error.NotArrow (
    NotArrow(..)
  , AsNotArrow(..)
  , notArrowInput
  , notArrowSrcLocInput
  ) where

import Control.Lens (preview)
import Control.Lens.Prism (Prism', prism)
import Text.Trifecta.Rendering (Renderable(..))
import           Text.Trifecta.Result               (Err (..), explain)
import           Text.PrettyPrint.ANSI.Leijen       (Doc, hang, text, (<+>))
import qualified Text.PrettyPrint.ANSI.Leijen       as PP ((<$>))
import qualified Data.Set as S (singleton)

import Common.Pretty (prettyToString)
import Component.Type.Note (AsNoteType(..), WithNoteType)
import Component.Type.Error (TypeErrorInput(..))
import Component.Type.Error.Pretty (PrettyTypeErrorRule(..), PrettyTypeErrorInput(..))

data NotArrow ty n a =
  NotArrow (ty n) (ty n)
  deriving (Eq, Ord, Show)

class AsNotArrow e ty | e -> ty where
  _NotArrow :: Prism' (e n a) (ty n, ty n)

instance AsNotArrow (NotArrow ty) ty where
  _NotArrow = prism (uncurry NotArrow) $ \x ->
    case x of
      NotArrow y z -> Right (y, z)

prettyNotArrow' :: (ty n -> Doc)
                  -> (ty n, ty n)
                  -> Doc
prettyNotArrow' prettyType (ty1, ty2) =
  hang 2 (text "Expected a function:" PP.<$>
          text "actual:" <+> prettyType ty1 PP.<$>
          text "expected:" <+> prettyType ty2 <+> text "-> ???")

prettyNotArrowSrcLoc' :: Renderable n
                        => (ty n -> Doc)
                        -> (n, ty n)
                        -> ty n
                        -> Doc
prettyNotArrowSrcLoc' prettyType (n, ac) ex =
  explain
    (render n)
      (Err
        (Just (msg <+> prettyType ac))
        []
        (S.singleton ((prettyToString . prettyType $ ex) ++ " -> ???")))
  where
    msg = text "Expected a function:"

prettyNotArrow :: AsNotArrow e ty
                 => (ty n -> Doc)
                 -> e n String
                 -> Maybe Doc
prettyNotArrow prettyType =
  fmap (prettyNotArrow' prettyType) .
  preview _NotArrow

prettyNotArrowSrcLoc :: ( AsNotArrow e ty
                        , WithNoteType ty
                        , Renderable n
                        )
                     => (ty n -> Doc)
                     -> e n String
                     -> Maybe Doc
prettyNotArrowSrcLoc prettyType =
    let
      prettyNotArrow'' (ac, ex) =
        maybe (prettyNotArrow' prettyType (ac, ex)) (\ac' -> prettyNotArrowSrcLoc' prettyType ac' ex) $
        preview _TyNote ac
    in
      fmap prettyNotArrow'' .
      preview _NotArrow

notArrowInput :: AsNotArrow e ty
              => TypeErrorInput e ty
notArrowInput =
  TypeErrorInput
    (PrettyTypeErrorInput [PrettyTypeErrorWithType prettyNotArrow])

notArrowSrcLocInput :: ( AsNotArrow e ty
                       , WithNoteType ty
                       )
                    => TypeErrorInput e ty
notArrowSrcLocInput =
  TypeErrorInput
    (PrettyTypeErrorInput [PrettyTypeErrorWithTypeSrcLoc prettyNotArrowSrcLoc])
