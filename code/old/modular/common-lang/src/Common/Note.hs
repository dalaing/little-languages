{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Common.Note (
    TranslateNote(..)
  ) where

-- TODO should this be a lens?
class TranslateNote nTm nTy where
  translateNote :: nTm -> nTy

instance TranslateNote a a where
  translateNote = id
