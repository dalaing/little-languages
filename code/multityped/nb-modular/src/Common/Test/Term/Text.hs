module Common.Test.Term.Text where

import Control.Lens (view)

import Test.Tasty
import Test.Tasty.QuickCheck

import Common.Term
import Common.Term.Gen
import Common.Term.Parse
import Common.Term.Pretty

textTests :: ( Eq tm
             , Show tm
             )
          => TermOutput e ty tm
          -> TestTree
textTests t =
  testGroup "text"
    [testProperty "roundTrip" (propRoundTrip t)]

propRoundTrip :: ( Eq tm
                 , Show tm
                 )
              => TermOutput e ty tm
              -> Property
propRoundTrip t =
  forAllShrink (view genAnyTerm t) (view shrinkTerm t) $ \tm ->
    case roundTrip tm of
      Left _ -> property False
      Right u -> u === tm
  where
    roundTrip =
      view parseTermString t .
      view prettyTermString t
