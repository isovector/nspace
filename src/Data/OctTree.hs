{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData      #-}

module Data.OctTree
  ( Cube (..)
  , midpoint
  , subdivide
  , Oct (..)
  , OctTree (..)
  , cube
  , fill
  , elements
  , lookup
  , mkRegionByPow
  , query
  , fuse
  , overlay
  , volumize
  , Raw.corners
  ) where

import           Data.Coerce (coerce)
import           Data.Foldable
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Ap(..))
import           Data.OctTree.Internal (Oct(..), Tree(..), Cube(..), pattern Oct, unwrap, intersects, containsCube, getIntersect, containsPoint, normalize, sizeof)
import qualified Data.OctTree.Internal as Raw
import           Data.Semigroup (Last(..))
import           Data.Semilattice
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC.Base (liftA2)
import           Linear.V3
import           Prelude hiding (lookup)

midpoint :: (Fractional a) => Cube a -> V3 a
midpoint (Cube (V3 x y z) (V3 w h d)) = V3 (x + w / 2) (y + h / 2) (z + d / 2)

subdivide :: Fractional a => Cube a -> Oct (Cube a)
subdivide (Cube (V3 x y z) (V3 w h d)) =
  let halfw = w / 2
      halfh = h / 2
      halfd = d / 2
   in Oct
        (Cube (V3 x y z) (V3 halfw halfh halfd))
        (Cube (V3 (x + halfw) y z) $ V3 (w - halfw) halfh halfd)
        (Cube (V3 x (y + halfh) z) $ V3 halfw (h - halfh) halfd)
        (Cube (V3 (x + halfw) (y + halfh) z) $ V3 (w - halfw) (h - halfh) halfd)
        (Cube (V3 x y (z + halfd)) $ V3 halfw halfh (d - halfd))
        (Cube (V3 (x + halfw) y (z + halfd)) $ V3 (w - halfw) halfh (d - halfd))
        (Cube (V3 x (y + halfh) (z + halfd)) $ V3 halfw (h - halfh) (d - halfd))
        (Cube (V3 (x + halfw) (y + halfh) (z + halfd)) $ V3 (w - halfw) (h - halfh) (d - halfd))


data OctTree a = OctTree
  { qt_default  :: a
  , qt_root_pow :: Integer
  , qt_tree     :: Tree a
  }
  deriving stock (Show, Functor)
  deriving (Semigroup, Monoid) via (Ap OctTree a)

instance Eq a => Eq (OctTree a) where
  q1@(OctTree a m tr) == q2@(OctTree a' n tr') =
    case compare m n of
      LT -> realloc q1 == q2
      EQ -> a == a' && tr == tr'
      GT -> q1 == realloc q2


instance Applicative OctTree where
  pure a = OctTree a 0 $ pure a
  liftA2 fabc q1@(OctTree a m qta) q2@(OctTree b n qtb) =
    case compare m n of
      LT -> liftA2 fabc (realloc q1) q2
      EQ -> OctTree (fabc a b) m $ liftA2 fabc qta qtb
      GT -> liftA2 fabc q1 (realloc q2)




qt_region :: OctTree a -> Cube Rational
qt_region = mkRegionByPow . qt_root_pow


mkRegionByPow :: Integer -> Cube Rational
mkRegionByPow n =
  let side = 2 ^ n
   in Cube (pure (-side)) $ pure $ side * 2

doubleGo :: a -> Oct (Tree a) -> Tree a
doubleGo def (Oct tl0 tr0 bl0 br0 tl1 tr1 bl1 br1) = Split $
  Oct
    (Split (Oct a a a a a a
                 a tl0)) (Split (Oct a a a a a  a
                                     tr0 a))
    (Split (Oct a a a a a bl0
                 a a)) (Split (Oct a a a a br0 a
                                    a  a))
    (Split (Oct a a
                 a tl1 a a a a)) (Split (Oct a  a
                                     tr1 a a a a a))
    (Split (Oct a bl1
                 a a a a a a)) (Split (Oct br1 a
                                    a  a a a a a))
  where
    a = Fill def


realloc :: OctTree a -> OctTree a
realloc (OctTree a n q) = OctTree a (n + 1) $ doubleGo a $ unwrap q


-- scale :: OctTree a -> OctTree a
-- scale (OctTree a n q) = OctTree a (n + 1) q

powToContainRegion :: Cube Rational -> Integer
powToContainRegion (Cube (V3 x y z) (V3 w h d)) =
  maximum $ (0 :) $ fmap (ceiling @Double . logBase 2 . fromRational)
    [ abs x
    , abs $ x + w
    , abs y
    , abs $ y + h
    , abs z
    , abs $ z + d
    ]

sel :: (Fractional r, Ord r) => a -> a -> Maybe (Cube r) -> Cube r -> Tree a
sel def _ Nothing _ = pure def
sel def v (Just r) qu = fillImpl def v r qu

fillImpl :: (Fractional r, Ord r) => a -> a -> Cube r -> Cube r -> Tree a
fillImpl def v area r
  | containsCube area r = pure v
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      Split $ sel def v <$> subarea <*> subr
  | otherwise = pure def

cube :: a -> a -> Cube Rational -> OctTree a
cube def v (normalize -> r)
  | sizeof r == 0  = OctTree def (powToContainRegion r) $ pure def
  | otherwise = OctTree def (powToContainRegion r) $ fillImpl def v r $ mkRegionByPow (powToContainRegion r)

fill :: forall a. Cube Rational -> a -> OctTree a -> OctTree a
fill (normalize -> r) a q = liftA2 fromMaybe q (cube Nothing (Just a) r)

getLocationImpl :: V3 Rational -> Cube Rational -> Tree a -> Maybe a
getLocationImpl p r qt
  | containsPoint r p = case qt of
      Fill a -> Just a
      Split qu -> asum $ getLocationImpl p <$> subdivide r <*> qu
  | otherwise = Nothing

lookup :: V3 Rational -> OctTree a -> a
lookup v2 (OctTree a n q) = fromMaybe a $ getLocationImpl v2 (mkRegionByPow n) q

query :: Semilattice s => (a -> s) -> Cube Rational -> OctTree a -> s
query f (normalize -> area) (OctTree a n q)
  | containsCube r area = queryImpl f area r q
  | intersects r area = queryImpl f area r q /\ f a
  | otherwise = f a
  where
    r = mkRegionByPow n


queryImpl :: Semilattice s => (a -> s) -> Cube Rational -> Cube Rational -> Tree a -> s
queryImpl f area r (Fill a)
  | intersects area r = f a
  | otherwise = mempty
queryImpl f area r (Split qu)
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      fold $ sel2 f <$> subarea <*> subr <*> qu
  | otherwise = mempty

sel2 :: Semilattice s => (a -> s) -> Maybe (Cube Rational) -> Cube Rational -> Tree a -> s
sel2 _ Nothing _ _ = mempty
sel2 f (Just area) r q = queryImpl f area r q

-- atLeast :: Integer -> OctTree a -> OctTree a
-- atLeast m qt@(OctTree _ n _)
--   | n < m = atLeast m $ realloc qt
--   | otherwise = qt

volumize :: OctTree a -> [(Cube Rational, a)]
volumize (OctTree _ n q) = volumizeImpl (mkRegionByPow n) q

volumizeImpl :: Cube Rational -> Tree a -> [(Cube Rational, a)]
volumizeImpl r (Fill a) = pure (r, a)
volumizeImpl r (Split qu) = do
  let subr = subdivide r
  fold $ volumizeImpl <$> subr <*> qu

elements :: Ord a => OctTree a -> Set a
elements qt = S.insert (qt_default qt) $ query S.singleton (qt_region qt) qt

fuse :: Eq a => OctTree a -> OctTree a
fuse (OctTree a n qt) = OctTree a n $ Raw.fuse qt

overlay :: forall a. OctTree a -> OctTree a -> OctTree a
overlay qt qt' = coerce @(OctTree (Last a)) $ coerce qt <> coerce qt'

