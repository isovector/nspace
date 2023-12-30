{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.QuadTree.Internal where

import Control.Applicative (liftA2)
import Data.Maybe (isJust)
import Data.Monoid (Ap(..))
import GHC.Generics (Generic)
import Linear.V2
import Linear.V4


------------------------------------------------------------------------------
-- | An axis-aligned bounding box in 3-space.
data Rect a = Rect
  { r_pos  :: !(V2 a)
  , r_size :: !(V2 a)
  }
  deriving stock (Show, Read, Eq, Generic, Ord, Functor)


------------------------------------------------------------------------------
r_x, r_y, r_w, r_h :: Rect a -> a
r_x (Rect (V2 x _) (V2 _ _)) = x
r_y (Rect (V2 _ y) (V2 _ _)) = y
r_w (Rect (V2 _ _) (V2 w _)) = w
r_h (Rect (V2 _ _) (V2 _ h)) = h


------------------------------------------------------------------------------
-- | @'containsCube' c1 c2@ is true when @c2@ is inside or equal to @c1@.
containsCube :: (Num a, Ord a) => Rect a -> Rect a -> Bool
containsCube r1@(Rect (V2 bx by) (V2 bw bh)) r2@(Rect (V2 sx sy) (V2 sw sh)) =
  r1 == r2 ||
  and
    [ bx <= sx
    , by <= sy
    , sx + sw <= bx + bw
    , sy + sh <= by + bh
    ]


------------------------------------------------------------------------------
-- | Does the cube contain a given point?
containsPoint :: (Ord a, Num a) => Rect a -> V2 a -> Bool
containsPoint (Rect _ (V2 w h)) _
  | w <= 0 || h <= 0
  = False
containsPoint (Rect (V2 x y) (V2 w h)) (V2 tx ty) =
  and
    [ x <= tx
    , y <= ty
    , tx < x + w
    , ty < y + h
    ]

------------------------------------------------------------------------------
-- | Get the co-ordinates of the corners of a 'Cube'.
corners :: Num a => Rect a -> V4 (V2 a)
corners (Rect (V2 x y) (V2 w h)) =
  let p = V2 x y
      dx = V2 w 0
      dy = V2 0 h
   in fmap (p +) $ V4 0   dx         dy       (dx + dy)


data Free f a
  = Fill a
  | Split (f (Free f a))
  deriving (Functor, Foldable, Traversable, Generic)

deriving via Ap (Free f) a instance (Applicative f, Semigroup a) => Semigroup (Free f a)
deriving via Ap (Free f) a instance (Applicative f, Monoid    a) => Monoid    (Free f a)

deriving stock instance (Show a, Show (f (Free f a))) => Show (Free f a)

instance (Eq a, Eq (f (Free f a)), Applicative f) => Eq (Free f a) where
  Fill a   == Fill b    = a             == b
  Split qu == Split qu' = qu            == qu'
  Fill a   == Split qu  = pure (pure a) == qu
  Split qu == Fill a    = pure (pure a) == qu

instance Applicative f => Applicative (Free f) where
  pure = Fill
  liftA2 fabc (Fill a) (Fill b) = Fill $ fabc a b
  liftA2 fabc (Fill a) (Split qu) = Split $ fmap (fmap (fabc a)) qu
  liftA2 fabc (Split qu) (Fill b) = Split $ fmap (fmap (flip  fabc b)) qu
  liftA2 fabc (Split qu) (Split qu') = Split $ liftA2 (liftA2 fabc) qu qu'

instance Monad f => Monad (Free f) where
  Fill a >>= f = f a
  Split qu >>= f = Split $ fmap (>>= f) qu

type Tree = Free V4


normalize :: (Num a, Ord a) => Rect a -> Rect a
normalize q@(Rect (V2 x y) (V2 w h))
  | w < 0 = let w' = abs w in normalize $ Rect (V2 (x - w') y) $ V2 w' h
  | h < 0 = let h' = abs h in normalize $ Rect (V2 x (y - h')) $ V2 w h'
  | otherwise = q


intersects :: (Ord a, Num a) => Rect a -> Rect a -> Bool
intersects r1 r2 = isJust $ getIntersect r1 r2


sizeof :: Num a => Rect a -> a
sizeof (Rect _ (V2 w h)) = w * h


getIntersect :: (Ord a, Num a) => Rect a -> Rect a -> Maybe (Rect a)
getIntersect (normalize -> r1) (normalize -> r2)
 | sizeof r1 == 0 = Just r1
 | sizeof r2 == 0 = Just r2
 | otherwise =
  let x0 = max (r_x r1) (r_x r2)
      y0 = max (r_y r1) (r_y r2)
      x1 = min (r_x r1 + r_w r1) (r_x r2 + r_w r2)
      y1 = min (r_y r1 + r_h r1) (r_y r2 + r_h r2)
      w = x1 - x0
      h = y1 - y0
   in case 0 < w && 0 < h of
        True -> Just $ Rect (V2 x0 y0) (V2 w h)
        False -> Nothing


unwrap :: Tree a -> V4 (Tree a)
unwrap (Fill a) = pure $ pure a
unwrap (Split qu) = qu


fuse :: Eq a => Tree a -> Tree a
fuse (Fill a) = Fill a
fuse (Split q) = doFuse $ fmap fuse q


doFuse :: Eq a => V4 (Tree a) -> Tree a
doFuse (V4 (Fill a) (Fill b) (Fill c) (Fill d))
  | a == b
  , b == c
  , c == d
  = Fill a
doFuse q = Split q



