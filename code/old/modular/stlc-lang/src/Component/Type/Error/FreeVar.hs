{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE FunctionalDependencies#-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE FlexibleContexts#-}
module Component.Type.Error.FreeVar (
    FreeVar(..)
  , AsFreeVar(..)
  , freeVarInput
  ) where

import Control.Lens (preview)
import Control.Lens.Prism (Prism', prism)
import           Text.PrettyPrint.ANSI.Leijen       (Doc, text, (<+>))

import Component.Type.Error (TypeErrorInput(..))
import Component.Type.Error.Pretty (PrettyTypeErrorRule(..), PrettyTypeErrorInput(..))

data FreeVar n a =
  FreeVar a
  deriving (Eq, Ord, Show)

class AsFreeVar e where
  _FreeVar :: Prism' (e n a) a

instance AsFreeVar FreeVar where
  _FreeVar = prism FreeVar $ \x ->
    case x of
      FreeVar y -> Right y

prettyFreeVar' :: String
                -> Doc
prettyFreeVar' v =
  text "Could not find" <+> text v

prettyFreeVarSrcLoc :: AsFreeVar e
                    => e n String
                    -> Maybe Doc
prettyFreeVarSrcLoc =
  fmap prettyFreeVar' .
  preview _FreeVar

freeVarInput :: AsFreeVar e
             => TypeErrorInput e ty
freeVarInput =
  TypeErrorInput
    (PrettyTypeErrorInput [PrettyTypeErrorBase prettyFreeVarSrcLoc])
