{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Component.Term.Note.Eval.SmallStep (
    smallStepInput
  ) where

import Control.Lens (preview, review)

import Component.Term.Eval.SmallStep (SmallStepRule(..), SmallStepInput(..))
import Component.Term.Note (AsNoteTerm(..), WithNoteTerm)

smallStepTmNote :: WithNoteTerm tm n a
                => (tm a -> Maybe (tm a))
                -> tm a
                -> Maybe (tm a)
smallStepTmNote smallStep tm = do
  (n, tm1) <- preview _TmNote tm
  tm1' <- smallStep tm1
  return $ review _TmNote (n, tm1')

smallStepInput :: WithNoteTerm tm n a
           => SmallStepInput (tm a)
smallStepInput =
  SmallStepInput
    [SmallStepRecurse smallStepTmNote]
