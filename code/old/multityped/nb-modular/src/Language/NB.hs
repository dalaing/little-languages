module Language.NB where

import Text.Trifecta.Rendering (Span)

import Common (LanguageInput(..), LanguageOutput(..), mkLanguage)

import Language.NB.Term
import Language.NB.Type
import Language.NB.Type.Error

languageInput :: LanguageInput (Error Span (Type Span)) (Type Span) (Term (Type Span) Span)
languageInput =
  LanguageInput typeInput termInput

languageOutput :: LanguageOutput (Error Span (Type Span)) (Type Span) (Term (Type Span) Span)
languageOutput =
  mkLanguage languageInput
