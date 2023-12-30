{-# LANGUAGE StrictData #-}

module Data.OctTree.Naive
  ( module Data.OctTree.Naive
  ) where

import Linear.V3
import Control.Applicative (liftA2)
import Data.Maybe (isJust)
import Data.Monoid (Ap(..))
import GHC.Generics (Generic)


data Region a = Region
  { r_x :: !a
  , r_y :: !a
  , r_z :: !a
  , r_w :: !a
  , r_h :: !a
  , r_d :: !a
  }
  deriving stock (Show, Read, Eq, Generic, Ord, Functor)

containsRegion :: (Num a, Ord a) => Region a -> Region a -> Bool
containsRegion r1@(Region bx by bz bw bh bd) r2@(Region sx sy sz sw sh sd) =
  r1 == r2 ||
  and
    [ bx <= sx
    , by <= sy
    , bz <= sz
    , sx + sw <= bx + bw
    , sy + sh <= by + bh
    , sz + sd <= bz + bd
    ]

containsPoint :: (Ord a, Num a) => Region a -> V3 a -> Bool
containsPoint (Region _ _ _ w h d) _
  | w <= 0 || h <= 0 || d <= 0
  = False
containsPoint (Region x y z w h d) (V3 tx ty tz) =
  and
    [ x <= tx
    , y <= ty
    , z <= tz
    , tx < x + w
    , ty < y + h
    , tz < z + d
    ]

corners :: Num a => Region a -> Oct (V3 a)
corners (Region x y z w h d) =
  let p = V3 x y z
      dx = V3 w 0 0
      dy = V3 0 h 0
      dz = V3 0 0 d
   in fmap (p +) $ Oct 0   dx         dy       (dx + dy)
                       dz (dx + dz) (dy + dz) (dx + dy + dz)

data Oct a = Oct a a
                 a a

                 a a
                 a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Oct a

instance Applicative Oct where
  pure a = Oct a a a a a a a a
  liftA2 fabc (Oct a1 a2 a3 a4 a5 a6 a7 a8) ( Oct b1 b2 b3 b4 b5 b6 b7 b8)
    = Oct (fabc a1 b1) (fabc a2 b2) (fabc a3 b3) (fabc a4 b4) (fabc a5 b5) (fabc a6 b6) (fabc a7 b7) (fabc a8 b8)

data Tree a
  = Fill a
  | Split (Oct (Tree a))
  deriving (Show, Functor, Foldable, Traversable, Generic)

instance Eq a => Eq (Tree a) where
  Fill a   == Fill b    = a             == b
  Split qu == Split qu' = qu            == qu'
  Fill a   == Split qu  = pure (pure a) == qu
  Split qu == Fill a    = pure (pure a) == qu



instance Applicative Tree where
  pure = Fill
  liftA2 fabc (Fill a) (Fill b) = Fill $ fabc a b
  liftA2 fabc (Fill a) (Split qu) = Split $ fmap (fmap (fabc a)) qu
  liftA2 fabc (Split qu) (Fill b) = Split $ fmap (fmap (flip  fabc b)) qu
  liftA2 fabc (Split qu) (Split qu') = Split $ liftA2 (liftA2 fabc) qu qu'

instance Monad Tree where
  Fill a >>= f = f a
  Split qu >>= f = Split $ fmap (>>= f) qu


normalize :: (Num a, Ord a) => Region a -> Region a
normalize q@(Region x y z w h d)
  | w < 0 = let w' = abs w in normalize $ Region (x - w') z y w' h d
  | h < 0 = let h' = abs h in normalize $ Region x (y - h') z w h' d
  | d < 0 = let d' = abs d in normalize $ Region x y (z - d') w h d'
  | otherwise = q

intersects :: (Ord a, Num a) => Region a -> Region a -> Bool
intersects r1 r2 = isJust $ getIntersect r1 r2

sizeof :: Num a => Region a -> a
sizeof (Region _ _ _ w h d) = w * h * d

getIntersect :: (Ord a, Num a) => Region a -> Region a -> Maybe (Region a)
getIntersect (normalize -> r1) (normalize -> r2)
 | sizeof r1 == 0 = Just r1
 | sizeof r2 == 0 = Just r2
 | otherwise =
  let x0 = max (r_x r1) (r_x r2)
      y0 = max (r_y r1) (r_y r2)
      z0 = max (r_z r1) (r_z r2)
      x1 = min (r_x r1 + r_w r1) (r_x r2 + r_w r2)
      y1 = min (r_y r1 + r_h r1) (r_y r2 + r_h r2)
      z1 = min (r_z r1 + r_d r1) (r_z r2 + r_d r2)
      w = x1 - x0
      h = y1 - y0
      d = z1 - z0
   in case 0 < w && 0 < h && 0 < d of
        True -> Just $ Region x0 y0 z0 w h d
        False -> Nothing


unwrap :: Tree a -> Oct (Tree a)
unwrap (Fill a) = pure $ pure a
unwrap (Split qu) = qu




fuse :: Eq a => Tree a -> Tree a
fuse (Fill a) = Fill a
fuse (Split q) = doFuse $ fmap fuse q

doFuse :: Eq a => Oct (Tree a) -> Tree a
doFuse (Oct (Fill a) (Fill b) (Fill c) (Fill d) (Fill e) (Fill f) (Fill g) (Fill h))
  | a == b
  , b == c
  , c == d
  , d == e
  , e == f
  , f == g
  , g == h
  = Fill a
doFuse q = Split q


deriving via Ap Tree a instance Semigroup a => Semigroup (Tree a)
deriving via Ap Tree a instance Monoid    a => Monoid    (Tree a)

