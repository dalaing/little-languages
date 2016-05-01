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

import           Control.Lens.TH                        (makeClassy)

import           Common.Parse                           (ParserHelperOutput)
import           Component.Term                         (HasTermOutput (..),
                                                         TermInput (..),
                                                         TermOutput (..),
                                                         mkTerm)
import           Component.Term.Eval.BigStep            (HasBigStepOutput (..))
import           Component.Term.Eval.SmallStep          (HasSmallStepOutput (..))
import           Component.Term.Eval.Value              (HasValueOutput (..))
import           Component.Term.Gen                     (HasGenTermOutput (..))
import           Component.Term.Infer                   (HasInferOutput (..))
import           Component.Term.Note.Strip              (StripNoteTerm)
import           Component.Term.Parse                   (HasParseTermOutput (..))
import           Component.Term.Pretty                  (HasPrettyTermOutput (..))
import           Component.Term.SubTerm                 (HasSubTermOutput (..))
import           Component.Type                         (HasTypeOutput (..),
                                                         TypeInput (..),
                                                         TypeOutput (..),
                                                         mkType)
import           Component.Type.Error                   (HasTypeErrorOutput (..),
                                                         TypeErrorInput,
                                                         TypeErrorOutput,
                                                         mkTypeError)
import           Component.Type.Error.Pretty            (HasPrettyTypeErrorOutput (..))
import           Component.Type.Error.UnknownType.Class (AsUnknownType)
import           Component.Type.Gen                     (HasGenTypeOutput (..))
import           Component.Type.Note.Strip              (StripNoteType)
import           Component.Type.Parse                   (HasParseTypeOutput (..))
import           Component.Type.Pretty                  (HasPrettyTypeOutput (..))

data ComponentInput r e ty nTy tm nTm a =
  ComponentInput {
    _typeInput      :: TypeInput ty nTy
  , _typeErrorInput :: TypeErrorInput e ty nTy
  , _termInput      :: TermInput r e ty nTy tm nTm a
  }

instance Monoid (ComponentInput r e ty nTy tm nTm a) where
  mempty =
    ComponentInput mempty mempty mempty
  mappend (ComponentInput ty1 te1 tm1) (ComponentInput ty2 te2 tm2) =
    ComponentInput (mappend ty1 ty2) (mappend te1 te2) (mappend tm1 tm2)

data ComponentOutput r e ty nTy tm nTm a =
  ComponentOutput {
    _cTypeOutput      :: TypeOutput ty nTy
  , _cTypeErrorOutput :: TypeErrorOutput e
  , _cTermOutput      :: TermOutput r e ty nTy tm nTm a
  }

makeClassy ''ComponentOutput

instance HasGenTypeOutput (ComponentOutput r e ty nTy tm nTm a) ty nTy where
  genTypeOutput = cTypeOutput . toGenTypeOutput

instance HasParseTypeOutput (ComponentOutput r e ty nTy tm nTm a) ty nTy where
  parseTypeOutput = cTypeOutput . toParseTypeOutput

instance HasPrettyTypeOutput (ComponentOutput r e ty nTy tm nTm a) ty where
  prettyTypeOutput = cTypeOutput . toPrettyTypeOutput

instance HasPrettyTypeErrorOutput (ComponentOutput r e ty nTy tm nTm a) e where
  prettyTypeErrorOutput = cTypeErrorOutput . toePrettyTypeErrorOutput

instance HasSubTermOutput (ComponentOutput r e ty nTy tm nTm a) tm nTm a where
  subTermOutput = cTermOutput . toSubTermOutput

instance HasGenTermOutput (ComponentOutput r e ty nTy tm nTm a) ty nTy tm nTm a where
  genTermOutput = cTermOutput . toGenTermOutput

instance HasParseTermOutput (ComponentOutput r e ty nTy tm nTm a) tm nTm a where
  parseTermOutput = cTermOutput . toParseTermOutput

instance HasPrettyTermOutput (ComponentOutput r e ty nTy tm nTm a) tm nTm a where
  prettyTermOutput = cTermOutput . toPrettyTermOutput

instance HasInferOutput (ComponentOutput r e ty nTy tm nTm a) r e ty nTy tm nTm a where
  inferOutput = cTermOutput . toInferOutput

instance HasValueOutput (ComponentOutput r e ty nTy tm nTm a) tm nTm a where
  valueOutput = cTermOutput . toValueOutput

instance HasSmallStepOutput (ComponentOutput r e ty nTy tm nTm a) tm nTm a where
  smallStepOutput = cTermOutput . toSmallStepOutput

instance HasBigStepOutput (ComponentOutput r e ty nTy tm nTm a) tm nTm a where
  bigStepOutput = cTermOutput . toBigStepOutput

mkComponent :: ( AsUnknownType e
               , Eq (tm nTm a)
               , StripNoteTerm tm tm
               , StripNoteType ty ty
               )
            => ParserHelperOutput
            -> ParserHelperOutput
            -> ComponentInput r e ty nTy tm nTm a
            -> ComponentOutput r e ty nTy tm nTm a
mkComponent pty ptm (ComponentInput ty te tm) =
  let
    tyO = mkType pty ty
  in
    ComponentOutput
      tyO
      (mkTypeError tyO te)
      (mkTerm ptm tyO tm)
