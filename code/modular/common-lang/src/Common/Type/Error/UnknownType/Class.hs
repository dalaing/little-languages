{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Common.Type.Error.UnknownType.Class (
    AsUnknownType(..)
  ) where

import Control.Lens.Prism (Prism')

class AsUnknownType t where
  _UnknownType :: Prism' t ()
