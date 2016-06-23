{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Common where

import Control.Lens.TH (makeClassy)

import Test.Tasty

import Common.Term
import Common.Type
import Common.Type.Error
import Common.Test
import Common.Repl

data LanguageInput e ty tm =
  LanguageInput {
    _liTypeInput :: TypeInput e ty
  , _liTermInput :: TermInput e ty tm
  }

makeClassy ''LanguageInput

instance HasTypeInput (LanguageInput e ty tm) e ty where
  typeInput = liTypeInput

instance HasTermInput (LanguageInput e ty tm) e ty tm where
  termInput = liTermInput

data LanguageOutput e ty tm =
  LanguageOutput {
    _loTypeOutput :: TypeOutput e ty
  , _loTermOutput :: TermOutput e ty tm
  , _tests :: TestTree
  , _repl :: IO ()
  }

makeClassy ''LanguageOutput

instance HasTypeOutput (LanguageOutput e ty tm) e ty where
  typeOutput = loTypeOutput

instance HasTermOutput (LanguageOutput e ty tm) e ty tm where
  termOutput = loTermOutput

mkLanguage :: ( Eq ty
              , Show ty
              , Eq tm
              , Show tm
              , AsUnknownType e n
              )
           => LanguageInput e ty tm
           -> LanguageOutput e ty tm
mkLanguage (LanguageInput tyI tmI) =
  let
    tyO = mkType tyI
    tmO = mkTerm tyO tmI
  in
    LanguageOutput
      tyO
      tmO
      (mkTests tyO tmO)
      (mkRepl tyO tmO)
