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
                => (tm n a -> Maybe (tm n a))
                -> tm n a
                -> Maybe (tm n a)
smallStepTmNote smallStep tm = do
  (_, tm1) <- preview _TmNote tm
  smallStep tm1

smallStepInput :: WithNoteTerm tm n a
           => SmallStepInput tm n a
smallStepInput =
  SmallStepInput
    [SmallStepRecurse smallStepTmNote]
