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
module Component.Term.Parent (
    AsParentTerm(..)
  ) where

import Control.Lens.Prism (Prism')

class AsParentTerm t p | t -> p where
  _ParentTerm :: Prism' (p n a) (t n a)
