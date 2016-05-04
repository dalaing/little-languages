{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Bound2 (
    Bound2(..)
  , Bound3(..)
  ) where

class Bound2 t where
  (>>>>=) :: Monad (f n) => t f n a -> (a -> f n b) -> t f n b

class Bound3 t where
  (>>>>>=) :: Monad (f m n) => t f m n a -> (a -> f m n b) -> t f m n b
