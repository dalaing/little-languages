{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Loc where

import Control.Lens (preview, review)
import Control.Lens.Iso (Iso', iso)
import Control.Lens.Prism (Prism', aside)

class WithLoc f where
  _Loc :: Prism' (f l) (l, f l)

  stripLoc :: f l -> f ()

  _WithLoc :: Iso' (f l) (Maybe l, f l)
  _WithLoc = iso there back
    where
      there t = case preview _Loc t of
        Just (l, t') -> (Just l, t')
        Nothing -> (Nothing, t)
      back (Nothing, t) = t
      back (Just l, t) = review _Loc (l, t)

withLoc :: WithLoc f => Prism' (f l) a -> f l -> Maybe (Maybe l, a)
withLoc p = preview (_WithLoc . aside p)
