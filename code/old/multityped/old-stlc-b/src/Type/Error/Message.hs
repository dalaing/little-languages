module Type.Error.Message where

import qualified Data.Set as S

import Text.Trifecta.Rendering
import Text.Trifecta.Result

import Text.PrettyPrint.ANSI.Leijen

import Type.Error

-- TODO break these out so that different components can have their own messages

toErr :: TypeError l a -> Err
toErr (TeUnexpected (TypeInfo ac _) ex) =
  Err (Just (text "unexpected type" <+> text (show ac))) [] (S.singleton (show ex))
toErr (TeExpectedEq (TypeInfo t1 _) (TypeInfo t2 _)) =
  mempty -- TODO
toErr (TeExpectedArr (TypeInfo t _)) =
  mempty -- TODO
toErr (TeUnknownVar _ v) =
  mempty -- TODO
toErr (TeUnknownType _) =
  mempty -- TODO

toDoc :: Renderable l => TypeError l a -> Doc
toDoc te@(TeUnexpected (TypeInfo _ l) _) =
  explain (render l) (toErr te)
toDoc te@(TeExpectedEq (TypeInfo _ l1) (TypeInfo _ l2)) =
  -- TODO can we also underline l1 in the message?
  explain (render l2) (toErr te)
toDoc te@(TeExpectedArr (TypeInfo _ l)) =
  explain (render l) (toErr te)
toDoc te@(TeUnknownVar l _) =
  explain (render l) (toErr te)
toDoc te@(TeUnknownType l) =
  explain (render l) (toErr te)
