{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Loc where

import Control.Lens (preview, review)
import Control.Lens.Iso (Iso', iso)
import Control.Lens.Prism (Prism', aside)

class WithLoc f where
  type Loc f
  type Without f

  _Loc :: Prism' f (Loc f, f)

  stripLoc :: f -> Without f

  _WithLoc :: Iso' f (Maybe (Loc f), f)
  _WithLoc = iso there back
    where
      there t = case preview _Loc t of
        Just (l, t') -> (Just l, t')
        Nothing -> (Nothing, t)
      back (Nothing, t) = t
      back (Just l, t) = review _Loc (l, t)

withLoc :: WithLoc f
        => Prism' f a
        -> f
        -> Maybe (Maybe (Loc f), a)
withLoc p = preview (_WithLoc . aside p)
