{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Component.Term.Note.Infer (
    inferInput
  ) where

import           Control.Lens         (preview, review)

import           Common.Note          (TranslateNote (..))
import           Component.Term.Infer (InferInput (..), InferRule (..))
import           Component.Term.Note  (AsNoteTerm (..), WithNoteTerm)
import           Component.Type.Note  (AsNoteType (..), WithNoteType)

inferTmNote :: ( WithNoteType ty
               , WithNoteTerm tm
               , TranslateNote nTm nTy
               , Monad m
               )
             => (ty nTy -> ty nTy)
             -> (tm nTm a -> m (ty nTy))
             -> tm nTm a
             -> Maybe (m (ty nTy))
inferTmNote _ infer =
    fmap inferTmNote' .
    preview _TmNote
  where
    inferTmNote' (n, tm) = do
      ty <- infer tm
      return $ review _TyNote (translateNote n, ty)

inferInput :: ( WithNoteType ty
              , WithNoteTerm tm
              , TranslateNote nTm nTy
              )
            => InferInput r e ty nTy tm nTm a
inferInput =
  InferInput
    [InferRecurse inferTmNote]
