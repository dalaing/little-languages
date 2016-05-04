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
module Component.Term.Note.Strip (
  StripNoteTerm(..)
  ) where

class StripNoteTerm x p | x -> p where
  mapMaybeNoteTerm :: (nTm -> Maybe mTm) -> x nTy nTm a -> p nTy mTm a

  stripNoteTerm :: x nTy nTm a -> p nTy mTm a
  stripNoteTerm = mapMaybeNoteTerm (const Nothing)

