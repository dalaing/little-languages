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
import           Common.Type.Error.UnknownType.Class (AsUnknownType)
import           Component                           (ComponentInput (..),
                                                      ComponentOutput (..),
                                                      mkComponent)

data LanguageInput e ty tm a =
  LanguageInput {
    _typeParserHelper :: ParserHelperInput
  , _termParserHelper :: ParserHelperInput
  , _componentInput   :: ComponentInput e ty tm a
  }

mkLanguage :: ( AsUnknownType e
              , Eq e
              , Show e
              , Eq ty
              , Show ty
              , Eq (tm a)
              , Show (tm a)
              )
           => LanguageInput e ty tm a
           -> ComponentOutput e ty tm a
mkLanguage (LanguageInput pty ptm c@(ComponentInput ty _ tm)) =
  let
    pty' = mkParserHelper (reservedWords ty) pty
    ptm' = mkParserHelper (reservedWords tm) ptm
    co = mkComponent pty' ptm' c
  in
    co

mkLanguageDefaultParser :: ( AsUnknownType e
                           , Eq e
                           , Show e
                           , Eq ty
                           , Show ty
                           , Eq (tm a)
                           , Show (tm a)
                           )
                        => ComponentInput e ty tm a
                        -> ComponentOutput e ty tm a
mkLanguageDefaultParser c =
  mkLanguage (LanguageInput defaultTypeParserHelper defaultTermParserHelper c)
