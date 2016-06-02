{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : POSIX

Big step rules and helpers for the N language.

This module reexports 'Term.Eval.BigStep.Strict'.
There is also 'Term.Eval.BigStep.Lazy' if you need it.
-}
module Term.Eval.BigStep (
    module Term.Eval.BigStep.Strict
  ) where

-- local
import Term.Eval.BigStep.Strict
