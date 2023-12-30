{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData      #-}

module Data.OctTree.Internal where

import Control.Applicative (liftA2)
import Data.Maybe (isJust)
import Data.Monoid (Ap(..))
import GHC.Generics (Generic)
import Linear.V3


------------------------------------------------------------------------------
-- | An axis-aligned bounding box in 3-space.
data Cube a = Cube
  { r_pos :: !(V3 a)
  , r_size :: !(V3 a)
  }
  deriving stock (Show, Read, Eq, Generic, Ord, Functor)


------------------------------------------------------------------------------
r_x, r_y, r_z, r_w, r_h, r_d :: Cube a -> a
r_x (Cube (V3 x _ _) (V3 _ _ _)) = x
r_y (Cube (V3 _ y _) (V3 _ _ _)) = y
r_z (Cube (V3 _ _ z) (V3 _ _ _)) = z
r_w (Cube (V3 _ _ _) (V3 w _ _)) = w
r_h (Cube (V3 _ _ _) (V3 _ h _)) = h
r_d (Cube (V3 _ _ _) (V3 _ _ d)) = d


------------------------------------------------------------------------------
-- | @'containsCube' c1 c2@ is true when @c2@ is inside or equal to @c1@.
containsCube :: (Num a, Ord a) => Cube a -> Cube a -> Bool
containsCube r1@(Cube (V3 bx by bz) (V3 bw bh bd)) r2@(Cube (V3 sx sy sz) (V3 sw sh sd)) =
  r1 == r2 ||
  and
    [ bx <= sx
    , by <= sy
    , bz <= sz
    , sx + sw <= bx + bw
    , sy + sh <= by + bh
    , sz + sd <= bz + bd
    ]


------------------------------------------------------------------------------
-- | Does the cube contain a given point?
containsPoint :: (Ord a, Num a) => Cube a -> V3 a -> Bool
containsPoint (Cube _ (V3 w h d)) _
  | w <= 0 || h <= 0 || d <= 0
  = False
containsPoint (Cube (V3 x y z) (V3 w h d)) (V3 tx ty tz) =
  and
    [ x <= tx
    , y <= ty
    , z <= tz
    , tx < x + w
    , ty < y + h
    , tz < z + d
    ]

------------------------------------------------------------------------------
-- | Get the co-ordinates of the corners of a 'Cube'.
corners :: Num a => Cube a -> Oct (V3 a)
corners (Cube (V3 x y z) (V3 w h d)) =
  let p = V3 x y z
      dx = V3 w 0 0
      dy = V3 0 h 0
      dz = V3 0 0 d
   in fmap (p +) $ Oct 0   dx         dy       (dx + dy)
                       dz (dx + dz) (dy + dz) (dx + dy + dz)

data Quad a = Quad a a
                   a a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Quad a

instance Applicative Quad where
  pure a = Quad a a a a
  liftA2 fabc (Quad a1 a2 a3 a4) (Quad b1 b2 b3 b4)
    = Quad (fabc a1 b1) (fabc a2 b2) (fabc a3 b3) (fabc a4 b4)

------------------------------------------------------------------------------
-- | An 8-tuple of values.
data Oct a = Oct' (Quad a) (Quad a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving (Semigroup, Monoid) via Ap Oct a

pattern Oct :: a -> a -> a -> a -> a -> a -> a -> a -> Oct a
pattern Oct a b c d e f g h = Oct' (Quad a b c d) (Quad e f g h)
{-# COMPLETE Oct #-}

instance Applicative Oct where
  pure a = Oct' (pure a) (pure a)
  liftA2 fabc (Oct' a1 a2) (Oct' b1 b2)
    = Oct' (liftA2 fabc a1 b1) (liftA2 fabc a2 b2)


data Tree a
  = Fill a
  | Split (Oct (Tree a))
  deriving (Show, Functor, Foldable, Traversable, Generic)

deriving via Ap Tree a instance Semigroup a => Semigroup (Tree a)
deriving via Ap Tree a instance Monoid    a => Monoid    (Tree a)

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


normalize :: (Num a, Ord a) => Cube a -> Cube a
normalize q@(Cube (V3 x y z) (V3 w h d))
  | w < 0 = let w' = abs w in normalize $ Cube (V3 (x - w') y z) $ V3 w' h d
  | h < 0 = let h' = abs h in normalize $ Cube (V3 x (y - h') z) $ V3 w h' d
  | d < 0 = let d' = abs d in normalize $ Cube (V3 x y (z - d')) $ V3 w h d'
  | otherwise = q


intersects :: (Ord a, Num a) => Cube a -> Cube a -> Bool
intersects r1 r2 = isJust $ getIntersect r1 r2


sizeof :: Num a => Cube a -> a
sizeof (Cube _ (V3 w h d)) = w * h * d


getIntersect :: (Ord a, Num a) => Cube a -> Cube a -> Maybe (Cube a)
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
        True -> Just $ Cube (V3 x0 y0 z0) (V3 w h d)
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


