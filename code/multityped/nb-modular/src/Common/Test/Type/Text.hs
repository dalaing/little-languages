module Common.Test.Type.Text where

import Control.Lens (view)

import Test.Tasty
import Test.Tasty.QuickCheck

import Common.Type
import Common.Type.Gen
import Common.Type.Parse
import Common.Type.Pretty

textTests :: ( Eq ty
             , Show ty
             )
          => TypeOutput e ty
          -> TestTree
textTests t =
  testGroup "text"
    [testProperty "roundTrip" (propRoundTrip t)]

propRoundTrip :: ( Eq ty
                 , Show ty
                 )
              => TypeOutput e ty
              -> Property
propRoundTrip t =
  forAllShrink (view genAnyType t) (view shrinkType t) $ \tm ->
    case roundTrip tm of
      Left _ -> property False
      Right u -> u === tm
  where
    roundTrip =
      view parseTypeString t .
      view prettyTypeString t
