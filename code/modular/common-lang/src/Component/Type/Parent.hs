{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Component.Type.Parent (
    AsParentType(..)
  ) where

import Control.Lens.Prism (Prism')

class AsParentType t p | t -> p where
  _ParentType :: Prism' (p n) (t n)
