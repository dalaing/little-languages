{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Type where

import Control.Lens.TH
import Control.Lens.Wrapped

data TypeF l f =
    TyBool
  | TyArr f f
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TypeF

data Type l = Type { getType :: TypeF l (Type l) }
              deriving (Eq, Ord, Show)

makeWrapped ''Type

instance AsTypeF (Type l) l (Type l) where
  _TyBool = _Wrapped . _TyBool
  _TyArr = _Wrapped . _TyArr

