module Rules.GenShrink.Test where

import Test.QuickCheck

import Rules.Div
import Rules.GenShrink

data TestTerm = TestInt Int
              | TestAdd TestTerm TestTerm

genTestInt :: Gen TestTerm
genTestInt = TestInt <$> arbitrary

-- this is using proalt
shrinkTestInt :: Shrinkable TestTerm TestTerm
shrinkTestInt = Shrink $ \t -> case t of
  TestInt i -> (Synthetic .TestInt) <$> shrink i
  _ -> []

genShrinkInt :: GenShrink Int Int
genShrinkInt = fromArbitrary

genShrinkTestInt1 :: GenShrink TestTerm TestTerm
genShrinkTestInt1 = GenShrink genTestInt shrinkTestInt

-- we can void the right case, since we test for it elsewhere
genShrinkTestInt2 :: GenShrink TestTerm TestTerm
genShrinkTestInt2 = proalt scatter gather genShrinkInt _
  where
    scatter (TestInt x) = Left x
    scatter t = Right t
    gather (Left x) = TestInt x
    gather (Right x) = x

-- TODO do the re sizing here?
genTestAdd :: Gen TestTerm
genTestAdd = TestAdd <$> genTestTerm <*> genTestTerm

shrinkTestAdd :: Shrinkable TestTerm TestTerm
shrinkTestAdd = Shrink $ \t -> case t of
  TestAdd t1 t2 ->
    -- TODO this isn't quite right
    -- we also want more aggressive shrinking to be first
    let
      s1 = shrShrink shrinkTestTerm t1
      s2 = shrShrink shrinkTestTerm t2
    in
      TestAdd <$> s1 <*> s2 ++ (t1 : s1) ++ (t2 : s2)
  _ -> []

genShrinkTestAdd1 :: GenShrink TestTerm TestTerm
genShrinkTestAdd1 = GenShrink genTestAdd shrinkTestAdd

-- TODO
-- we want to unpack TestAdd twice with profix
-- we can void the right case, since we test for it elsewhere
genShrinkTestAdd2 :: GenShrink TestTerm TestTerm
genShrinkTestAdd2 = proalt scatter1 gather1 (proap scatter2 TestAdd genShrinkTestTerm2 genShrinkTestTerm2) _
  where
    scatter1 (TestAdd x y) = Left (TestAdd x y)
    scatter1 t = Right t
    gather1 (Left (TestAdd x y)) = TestAdd x y
    gather1 (Right x) = x
    scatter2 (TestAdd x y) = (x, y)

genTestTerm :: Gen TestTerm
genTestTerm = sized genTestTerm'
  where
    genTestTerm' 0 = genTestInt
    genTestTerm' s = oneof [ genTestInt, resize (s `div` 2) genTestAdd]

shrinkTestTerm :: Shrinkable TestTerm TestTerm
shrinkTestTerm = Shrink $ \t -> case t of
  TestAdd x y -> shrShrink shrinkTestAdd (TestAdd x y)
  TestInt i -> shrShrink shrinkTestInt (TestInt i)

genShrinkTestTerm1 :: GenShrink TestTerm TestTerm
genShrinkTestTerm1 = GenShrink genTestTerm shrinkTestTerm

genShrinkTestTerm2 :: GenShrink TestTerm TestTerm
genShrinkTestTerm2 = proalt scatter gather genShrinkTestInt2 genShrinkTestAdd2
  where
    scatter (TestInt i) = Left (TestInt i)
    scatter (TestAdd x y) = Right (TestAdd x y)
    gather (Left x) = x
    gather (Right x) = x
