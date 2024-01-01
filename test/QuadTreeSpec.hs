{-# OPTIONS_GHC -Wno-orphans #-}

module QuadTreeSpec where

import Data.Foldable
import Data.Monoid (Sum)
import Data.QuadTree hiding (elements)
import Data.QuadTree.Internal (rectContainsPoint, Free(..))
import Data.Ratio
import Data.Semigroup (Any(..), Max(..))
import Data.Semilattice
import Prelude hiding (lookup)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (Max a) where
  arbitrary = Max <$> arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (V4 a) where
  arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (Rect a) where
  arbitrary = Rect <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (V2 a) where
  arbitrary = V2 <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance EqProp a => EqProp (QuadTree a) where
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

instance Arbitrary a => Arbitrary (QuadTree a) where
  arbitrary = QuadTree <$> arbitrary <*> elements [0..10] <*> arbitrary

observe :: QuadTree a -> V2 Rational -> a
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
      =-= \p -> if rectContainsPoint r p then a else observe q p

  prop "fuse" $ \(q :: QuadTree (Sum Int)) ->
    observe (fuse q)
      =-= observe q

  prop "query" $ \(applyFun -> f :: Bool -> Any) q x y w h ->
    let r = Rect (V2 x y) (V2 w h) in
      query f r q =-= f (lookup (midpoint r) q) /\
        foldr1 (/\) ((flip (query f) q) <$> subdivide r)

  prop "toRects" $ \(ot :: QuadTree Int) -> do
    ot =-= foldr (uncurry fill) (pure $ defaultValue ot) (toRects ot)


  prop "mempty" $
    observe (mempty @(QuadTree (Sum Int)))
      =-= mempty

  prop "<>" $ \(q1 :: QuadTree (Sum Int)) q2 ->
    observe (q1 <> q2)
      =-= observe q1 <> observe q2

  propBatch "QuadTree Semigroup" $ semigroup ((0 :: Max Int), (0 :: Int))
  propBatch "QuadTree Monoid" $ monoid ((0 :: Max Int), ([] :: [Int]))
  propBatch "QuadTree Applicative" $ applicative (undefined :: QuadTree (Int, Int, Int))

