{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Note.Eval.Value (
    valueInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.Value (ValueRule(..), ValueInput(..))
import Component.Term.Note (AsNoteTerm(..), WithNoteTerm)

valueTmNote :: WithNoteTerm tm
            => (tm nTy nTm a -> Maybe (tm nTy nTm a))
            -> tm nTy nTm a
            -> Maybe (tm nTy nTm a)
valueTmNote value tm = do
  (_, tm1) <- preview _TmNote tm
  value tm1

valueInput :: WithNoteTerm tm
           => ValueInput tm 
valueInput =
  ValueInput
    [ValueRecurse valueTmNote]
