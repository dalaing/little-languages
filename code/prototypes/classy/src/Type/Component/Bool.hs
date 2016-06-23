module Type.Component.Bool where

import Control.Lens

class AsTyBool t where
  _TyBool :: Prism' t ()
