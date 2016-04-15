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
{-# LANGUAGE TemplateHaskell        #-}
module Component (
    ComponentInput(..)
  , ComponentOutput(..)
  , mkComponent
  ) where

import           Control.Lens.TH                     (makeClassy)

import           Common.Parse                        (ParserHelperOutput)
import           Common.Type.Error.UnknownType.Class (AsUnknownType)
import           Component.Term                      (HasTermOutput (..),
                                                      TermInput (..),
                                                      TermOutput (..), mkTerm)
import           Component.Term.Eval.BigStep         (HasBigStepOutput (..))
import           Component.Term.Eval.SmallStep       (HasSmallStepOutput (..))
import           Component.Term.Eval.Value           (HasValueOutput (..))
import           Component.Term.Gen                  (HasGenTermOutput (..))
import           Component.Term.Infer                (HasInferOutput (..))
import           Component.Term.Parse                (HasParseTermOutput (..))
import           Component.Term.Pretty               (HasPrettyTermOutput (..))
import           Component.Term.Size                 (HasTermSizeOutput (..))
import           Component.Type                      (HasTypeOutput (..),
                                                      TypeInput (..),
                                                      TypeOutput (..), mkType)
import           Component.Type.Error                (HasTypeErrorOutput (..),
                                                      TypeErrorInput,
                                                      TypeErrorOutput,
                                                      mkTypeError)
import           Component.Type.Error.Pretty         (HasPrettyTypeErrorOutput (..))
import           Component.Type.Gen                  (HasGenTypeOutput (..))
import           Component.Type.Parse                (HasParseTypeOutput (..))
import           Component.Type.Pretty               (HasPrettyTypeOutput (..))

data ComponentInput e ty tm a =
  ComponentInput {
    _typeInput      :: TypeInput ty
  , _typeErrorInput :: TypeErrorInput e ty
  , _termInput      :: TermInput e ty tm a
  }

instance Monoid (ComponentInput e ty tm a) where
  mempty =
    ComponentInput mempty mempty mempty
  mappend (ComponentInput ty1 te1 tm1) (ComponentInput ty2 te2 tm2) =
    ComponentInput (mappend ty1 ty2) (mappend te1 te2) (mappend tm1 tm2)

data ComponentOutput e ty tm a =
  ComponentOutput {
    _cTypeOutput      :: TypeOutput ty
  , _cTypeErrorOutput :: TypeErrorOutput e ty
  , _cTermOutput      :: TermOutput e ty tm a
  }

makeClassy ''ComponentOutput

instance HasGenTypeOutput (ComponentOutput e ty tm a) ty where
  genTypeOutput = cTypeOutput . toGenTypeOutput

instance HasParseTypeOutput (ComponentOutput e ty tm a) ty where
  parseTypeOutput = cTypeOutput . toParseTypeOutput

instance HasPrettyTypeOutput (ComponentOutput e ty tm a) ty where
  prettyTypeOutput = cTypeOutput . toPrettyTypeOutput

instance HasPrettyTypeErrorOutput (ComponentOutput e ty tm a) e where
  prettyTypeErrorOutput = cTypeErrorOutput . toePrettyTypeErrorOutput

instance HasTermSizeOutput (ComponentOutput e ty tm a) (tm a) where
  termSizeOutput = cTermOutput . toTermSizeOutput

instance HasGenTermOutput (ComponentOutput e ty tm a) (tm a) where
  genTermOutput = cTermOutput . toGenTermOutput

instance HasParseTermOutput (ComponentOutput e ty tm a) tm a where
  parseTermOutput = cTermOutput . toParseTermOutput

instance HasPrettyTermOutput (ComponentOutput e ty tm a) (tm a) where
  prettyTermOutput = cTermOutput . toPrettyTermOutput

instance HasInferOutput (ComponentOutput e ty tm a) e ty (tm a) where
  inferOutput = cTermOutput . toInferOutput

instance HasValueOutput (ComponentOutput e ty tm a) (tm a) where
  valueOutput = cTermOutput . toValueOutput

instance HasSmallStepOutput (ComponentOutput e ty tm a) (tm a) where
  smallStepOutput = cTermOutput . toSmallStepOutput

instance HasBigStepOutput (ComponentOutput e ty tm a) (tm a) where
  bigStepOutput = cTermOutput . toBigStepOutput

mkComponent :: AsUnknownType e
            => ParserHelperOutput
            -> ParserHelperOutput
            -> ComponentInput e ty tm a
            -> ComponentOutput e ty tm a
mkComponent pty ptm (ComponentInput ty te tm) =
  let
    tyO = mkType pty ty
  in
    ComponentOutput
      tyO
      (mkTypeError tyO te)
      (mkTerm ptm tm)
