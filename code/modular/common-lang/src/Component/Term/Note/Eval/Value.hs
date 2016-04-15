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

valueTmNote :: WithNoteTerm tm n a
            => (tm a -> Maybe (tm a))
            -> tm a
            -> Maybe (tm a)
valueTmNote value tm = do
  (n, tm1) <- preview _TmNote tm
  tm1' <- value tm1
  return $ review _TmNote (n, tm1')

valueInput :: WithNoteTerm tm n a
           => ValueInput (tm a)
valueInput =
  ValueInput
    [ValueRecurse valueTmNote]
