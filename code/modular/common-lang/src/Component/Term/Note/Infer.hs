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

import           Component.Term.Infer (InferInput (..), InferRule (..))
import           Component.Term.Note  (AsNoteTerm (..), WithNoteTerm)
import           Component.Type.Note  (AsNoteType (..), WithNoteType)

inferTmNote :: ( WithNoteType n ty
               , WithNoteTerm tm n a
               , Monad m
               )
             => (tm a -> m ty)
             -> tm a
             -> Maybe (m ty)
inferTmNote infer =
    fmap inferTmNote' .
    preview _TmNote
  where
    inferTmNote' (n, tm) = do
      ty <- infer tm
      return $ review _TyNote (n, ty)

inferInput :: ( WithNoteType n ty
              , WithNoteTerm tm n a
              )
            => InferInput e ty (tm a)
inferInput =
  InferInput
    [InferRecurse inferTmNote]
