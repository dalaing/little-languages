module Type.Component.Arrow where

import Control.Lens

class AsTyArrow t where
  _TyArrow :: Prism' t (t, t)
