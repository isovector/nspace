{-# OPTIONS_GHC -Wno-orphans #-}

module OctTreeSpec where

import Data.Foldable
import Data.Monoid (Sum)
import Data.OctTree hiding (elements)
import Data.OctTree.Internal (cubeContainsPoint, Free(..))
import Data.Ratio
import Data.Semigroup (Any(..), Max(..))
import Data.Semilattice
import Prelude hiding (lookup)
import QuadTreeSpec ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (Oct a) where
  arbitrary = Oct <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (Cube a) where
  arbitrary = Cube <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance EqProp a => EqProp (OctTree a) where
  q1 =-= q2 = property $ \a -> flip lookup q1 a =-= flip lookup q2 a

instance Arbitrary a => Arbitrary (Free a) where
  arbitrary =
    let terminal = [Fill <$> arbitrary]
     in sized $ \n ->
          case n <= 1 of
            True -> oneof terminal
            False -> oneof $
              [ Split <$> Test.QuickCheck.scale (`div` 4) arbitrary
              ] <> terminal
  shrink = genericShrink

instance Arbitrary a => Arbitrary (OctTree a) where
  arbitrary = OctTree <$> arbitrary <*> elements [0..10] <*> arbitrary

observe :: OctTree a -> V3 Rational -> a
observe = flip lookup

propBatch :: String -> TestBatch -> Spec
propBatch s = describe s . traverse_ (uncurry prop) . unbatch

spec :: Spec
spec = modifyMaxSuccess (const 10000) $ do
  prop "empty" $ \(a :: Int) ->
    observe (pure a)
      =-= const a

  prop "fill" $ \r (a :: Int) q ->
    observe (fill r a q)
      =-= \p -> if cubeContainsPoint r p then a else observe q p

  prop "fuse" $ \(q :: OctTree (Sum Int)) ->
    observe (fuse q)
      =-= observe q

  prop "query" $ \(applyFun -> f :: Bool -> Any) q x y z w h d ->
    let r = Cube (V3 x y z) (V3 w h d) in
      query f r q =-= f (lookup (midpoint r) q) /\
        foldr1 (/\) ((flip (query f) q) <$> subdivide r)

  prop "tocubes" $ \(ot :: OctTree Int) -> do
    ot =-= foldr (uncurry fill) (pure $ defaultValue ot) (toCubes ot)


  prop "mempty" $
    observe (mempty @(OctTree (Sum Int)))
      =-= mempty

  prop "<>" $ \(q1 :: OctTree (Sum Int)) q2 ->
    observe (q1 <> q2)
      =-= observe q1 <> observe q2

  propBatch "OctTree Semigroup" $ semigroup ((0 :: Max Int), (0 :: Int))
  propBatch "OctTree Monoid" $ monoid ((0 :: Max Int), ([] :: [Int]))
  propBatch "OctTree Applicative" $ applicative (undefined :: OctTree (Int, Int, Int))

