module Type.Component.Int where

import Control.Lens

class AsTyInt t where
  _TyInt :: Prism' t ()
