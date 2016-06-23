{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Note.Gen (
    genTermInput
  ) where

import Component.Term.Gen (GenTermInput(..))

genTermInput :: GenTermInput ty tm
genTermInput = mempty
