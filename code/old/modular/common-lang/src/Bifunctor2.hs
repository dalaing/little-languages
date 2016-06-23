{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Bifunctor2 (
    Bifunctor2(..)
  ) where

import Data.Bifunctor (Bifunctor(..))
import Data.Proxy (Proxy)
import Data.Constraint (Dict)

class Bifunctor2 tm where
  bifunctor2 :: Proxy nTy -> Dict (Bifunctor (tm nTy))
