{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Component.Type.Error.ExpectedEq.Class (
    AsExpectedEq(..)
  ) where

import           Control.Lens.Prism (Prism')

class AsExpectedEq e ty | e -> ty where
  _ExpectedEq :: Prism' (e n a) (ty n, ty n)
