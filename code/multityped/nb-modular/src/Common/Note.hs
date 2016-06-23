{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Note where

import           Control.Lens       (preview, review)
import           Control.Lens.Iso   (Iso', iso)
import           Control.Lens.Prism (Prism', aside)

class WithoutNote f where
  type Without f
  stripNote :: f -> Without f

class WithNote f where
  type Note f

  _Note :: Prism' f (Note f, f)

  _WithNote :: Iso' f (Maybe (Note f), f)
  _WithNote = iso there back
    where
      there t = case preview _Note t of
        Just (l, t') -> (Just l, t')
        Nothing -> (Nothing, t)
      back (Nothing, t) = t
      back (Just l, t) = review _Note (l, t)

withNote :: WithNote f
        => Prism' f a
        -> f
        -> Maybe (Maybe (Note f), a)
withNote p = preview (_WithNote . aside p)
