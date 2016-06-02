{-|
Copyright   : (c) Dave Laing, 2016
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
-- from 'doctest'
import Test.DocTest

main :: IO ()
main = doctest $ "-isrc" :
  [ "Common.Parse"
  , "Common.Pretty"
  , "Term"
  , "Term.Eval.Value"
  , "Term.Eval.Value.Lazy"
  , "Term.Eval.Value.Strict"
  , "Term.Eval.SmallStep"
  , "Term.Eval.SmallStep.Lazy"
  , "Term.Eval.SmallStep.Strict"
  , "Term.Eval.BigStep"
  , "Term.Eval.BigStep.Lazy"
  , "Term.Eval.BigStep.Strict"
  , "Term.Infer"
  , "Term.Parse"
  , "Term.Pretty"
  , "Type"
  , "Type.Parse"
  , "Type.Pretty"
  , "Type.Error"
  , "Type.Error.Pretty"
  ]

