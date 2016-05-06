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
               , Monad m
               )
             => (ty n -> ty n)
             -> (tm n n a -> m (ty n))
             -> tm n n a
             -> Maybe (m (ty n))
inferTmNote _ infer =
    fmap inferTmNote' .
    preview _TmNote
  where
    inferTmNote' (n, tm) = do
      ty <- infer tm
      return $ review _TyNote (n, ty)

inferInput :: ( WithNoteType ty
              , WithNoteTerm tm
              )
            => InferInput r e ty tm
inferInput =
  InferInput
    [InferRecurse inferTmNote]
