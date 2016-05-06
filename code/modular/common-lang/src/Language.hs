{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Language (
    LanguageInput(..)
  , mkLanguage
  , mkLanguageDefaultParser
  ) where

import           Common.Parse                        (GetReservedWords (..),
                                                      ParserHelperInput,
                                                      defaultTermParserHelper,
                                                      defaultTypeParserHelper,
                                                      mkParserHelper)
import           Component.Type.Error.UnknownType.Class (AsUnknownType)
import           Component                           (ComponentInput (..),
                                                      ComponentOutput (..),
                                                      mkComponent)
import Component.Term.Note (WithNoteTerm)
import Component.Type.Note (WithNoteType)
import Component.Term.Note.Strip (StripNoteTerm)
import Component.Type.Note.Strip (StripNoteType)
import Extras (Eq1, Eq3, Show1, Show2, Show3)

data LanguageInput r e ty tm =
  LanguageInput {
    _typeParserHelper :: ParserHelperInput
  , _termParserHelper :: ParserHelperInput
  , _componentInput   :: ComponentInput r e ty tm
  }

mkLanguage :: ( AsUnknownType e
              , Eq3 tm
              , Eq1 ty
              , Show3 tm
              , Show1 ty
              , Show2 e
              , WithNoteTerm tm
              , WithNoteType ty
              , StripNoteTerm tm tm
              , StripNoteType ty ty
              )
           => LanguageInput r e ty tm
           -> ComponentOutput r e ty tm
mkLanguage (LanguageInput pty ptm c@(ComponentInput ty _ tm)) =
  let
    pty' = mkParserHelper (reservedWords ty) pty
    ptm' = mkParserHelper (reservedWords tm) ptm
    co = mkComponent pty' ptm' c
  in
    co

mkLanguageDefaultParser :: ( AsUnknownType e
                           , Eq3 tm
                           , Eq1 ty
                           , Show3 tm
                           , Show1 ty
                           , Show2 e
                           , WithNoteTerm tm
                           , WithNoteType ty
                           , StripNoteTerm tm tm
                           , StripNoteType ty ty
                           )
                        => ComponentInput r e ty tm
                        -> ComponentOutput r e ty tm
mkLanguageDefaultParser c =
  mkLanguage (LanguageInput defaultTypeParserHelper defaultTermParserHelper c)
