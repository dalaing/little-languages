{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Bound2 (
    Bound2(..)
  ) where

class Bound2 t where
  (>>>>=) :: Monad (f n) => t f n a -> (a -> f n b) -> t f n b
