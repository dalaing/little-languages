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

data LanguageInput r e ty nTy tm nTm a =
  LanguageInput {
    _typeParserHelper :: ParserHelperInput
  , _termParserHelper :: ParserHelperInput
  , _componentInput   :: ComponentInput r e ty nTy tm nTm a
  }

mkLanguage :: ( AsUnknownType e
              , Eq e
              , Show e
              , Eq (ty nTy)
              , Show (ty nTy)
              , Eq (tm nTm a)
              , Show (tm nTm a)
              )
           => LanguageInput r e ty nTy tm nTm a
           -> ComponentOutput r e ty nTy tm nTm a
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
                           , Eq (ty nTy)
                           , Show (ty nTy)
                           , Eq (tm nTm a)
                           , Show (tm nTm a)
                           )
                        => ComponentInput r e ty nTy tm nTm a
                        -> ComponentOutput r e ty nTy tm nTm a
mkLanguageDefaultParser c =
  mkLanguage (LanguageInput defaultTypeParserHelper defaultTermParserHelper c)
