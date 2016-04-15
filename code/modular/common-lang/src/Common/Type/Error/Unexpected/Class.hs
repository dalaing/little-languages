{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Common.Type.Error.Unexpected.Class (
    AsUnexpected(..)
  ) where

import           Control.Lens.Prism (Prism')

class AsUnexpected e ty | e -> ty where
  _Unexpected :: Prism' e (ty, ty)
