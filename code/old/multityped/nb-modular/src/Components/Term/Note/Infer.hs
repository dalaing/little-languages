module Components.Term.Note.Infer where

import Control.Lens (preview, review)

import Common.Recursion (MaybeStep(..))
import Common.Term.Infer

import Components.Type.Note.Data
import Components.Term.Note.Data

inferTmNoted :: ( WithNoteTerm n ty tm
                , WithNoteType n ty
                , Monad m
                )
             => (tm -> m ty)
             -> tm
             -> Maybe (m ty)
inferTmNoted step tm = do
  (n, tm') <- preview _TmNoted tm
  return $ inferTmNoted' step n tm'

inferTmNoted' :: ( WithNoteTerm n ty tm
                 , WithNoteType n ty
                 , Monad m
                 )
              => (tm -> m ty)
              -> n
              -> tm
              -> m ty
inferTmNoted' step n tm = do
  ty <- step tm
  return $ review _TyNoted (n, ty)

inferInput :: ( WithNoteTerm n ty tm
              , WithNoteType n ty
              )
           => InferInput e ty tm
inferInput =
  InferInput
    [ MSRecurse inferTmNoted ]
