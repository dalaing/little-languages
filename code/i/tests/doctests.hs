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
  , "Term.Eval.SmallStep"
  , "Term.Eval.BigStep"
  , "Term.Infer"
  , "Term.Parse"
  , "Term.Pretty"
  , "Type"
  , "Type.Parse"
  , "Type.Pretty"
  , "Type.Error"
  , "Type.Error.Pretty"
  ]

