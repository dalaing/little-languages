{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Common.Type.Error.Pretty (
    unknownTypeRule
  , unexpectedRule
  , expectedEqRule
  ) where

import Control.Lens (preview)
import Text.PrettyPrint.ANSI.Leijen(Doc, text, (<+>), hang)
import qualified Text.PrettyPrint.ANSI.Leijen as PP ((<$>))

import Common.Type.Error (AsUnknownType(..), AsUnexpected(..), AsExpectedEq(..))
import Component.Type.Error.Pretty (PrettyTypeErrorRule(..))

