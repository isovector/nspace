{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData      #-}

module Data.OctTree
  ( OctTree (..)

    -- * Constructing 'OctTree's
  , fill
  , combineAla

    -- * Spatially Querying 'OctTree's
  , lookup
  , query

    -- * Eliminating 'OctTree's
  , fuse
  , elements
  , toCubes
  , boundingCube
  , defaultValue

    -- * Constructing 'Cube's
  , Cube (..)
  , mkCubeByPow

    -- * Eliminating 'Cube's
  , midpoint
  , subdivide
  , Raw.cubeCorners

    -- * Indexing Types
  , V3 (..)
  , Oct (..)
  ) where

import           Data.Coerce
import           Data.Foldable
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Ap(..))
import           Data.OctTree.Internal (Free(..), Oct(..), Cube(..), pattern Oct8, unwrap, intersects, cubeContainsCube, getIntersect, cubeContainsPoint, normalize, cubeSize)
import qualified Data.OctTree.Internal as Raw
import           Data.Semilattice
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC.Base (liftA2)
import           Linear.V3
import           Prelude hiding (lookup)


------------------------------------------------------------------------------
-- | Compute the center of a 'Cube'.
midpoint :: (Fractional a) => Cube a -> V3 a
midpoint (Cube pos sz) = pos + sz / 2


------------------------------------------------------------------------------
-- | Subdivide a 'Cube' into eight 'Cube's which fill up the same volume.
subdivide :: Fractional a => Cube a -> Oct (Cube a)
subdivide (Cube (V3 x y z) (V3 w h d)) =
  let halfw = w / 2
      halfh = h / 2
      halfd = d / 2
   in Oct8
        (Cube (V3 x y z) (V3 halfw halfh halfd))
        (Cube (V3 (x + halfw) y z) $ V3 (w - halfw) halfh halfd)
        (Cube (V3 x (y + halfh) z) $ V3 halfw (h - halfh) halfd)
        (Cube (V3 (x + halfw) (y + halfh) z) $ V3 (w - halfw) (h - halfh) halfd)
        (Cube (V3 x y (z + halfd)) $ V3 halfw halfh (d - halfd))
        (Cube (V3 (x + halfw) y (z + halfd)) $ V3 (w - halfw) halfh (d - halfd))
        (Cube (V3 x (y + halfh) (z + halfd)) $ V3 halfw (h - halfh) (d - halfd))
        (Cube (V3 (x + halfw) (y + halfh) (z + halfd)) $ V3 (w - halfw) (h - halfh) (d - halfd))


------------------------------------------------------------------------------
-- | A type mapping values at (infinitely precise) locations in 3D
-- space. That is, you can consider an 'OctTree' to be a function @'V3'
-- 'Rational' -> a@, equipped with efficient means of querying the space.
--
-- 'OctTree's should usually be constructed using their 'Monoid'al or
-- 'Applicative' interfaces, as well as by way of the 'fill' function.
data OctTree a = OctTree
  { ot_default  :: a
  , ot_root_pow :: Integer
  , ot_tree     :: Free a
  }
  deriving stock (Show, Functor)
  deriving (Num, Semigroup, Monoid) via (Ap OctTree a)

instance Semilattice a => Semilattice (OctTree a)


------------------------------------------------------------------------------
-- | Get the value used to fill the infinity of space in an 'OctTree'.
defaultValue :: OctTree a -> a
defaultValue = ot_default

instance Eq a => Eq (OctTree a) where
  q1@(OctTree a m tr) == q2@(OctTree a' n tr') =
    case compare m n of
      LT -> realloc q1 == q2
      EQ -> a == a' && tr == tr'
      GT -> q1 == realloc q2


instance Applicative OctTree where
  pure a = OctTree a 0 $ pure a
  liftA2 fabc q1@(OctTree a m ota) q2@(OctTree b n otb) =
    case compare m n of
      LT -> liftA2 fabc (realloc q1) q2
      EQ -> OctTree (fabc a b) m $ liftA2 fabc ota otb
      GT -> liftA2 fabc q1 (realloc q2)


------------------------------------------------------------------------------
-- | Get a 'Cube' guaranteed to bound all of the non-defaulted values in the
-- 'OctTree'.
boundingCube :: OctTree a -> Cube Rational
boundingCube = mkCubeByPow . ot_root_pow


------------------------------------------------------------------------------
-- | Construct a 'Cube' centered around $(0, 0, 0)$, with side length $2n$.
mkCubeByPow :: Integer -> Cube Rational
mkCubeByPow n =
  let side = 2 ^ n
   in Cube (pure (-side)) $ pure $ side * 2


------------------------------------------------------------------------------
-- | Build a larger 'Free' 'Oct' by doubling each side length, keeping the
-- contents in the center.
doubleGo :: a -> Oct (Free a) -> Free a
doubleGo def (Oct8 tl0 tr0 bl0 br0 tl1 tr1 bl1 br1) = Split $
  Oct8
    (Split (Oct8 a a a a a a
                 a tl0)) (Split (Oct8 a a a a a  a
                                     tr0 a))
    (Split (Oct8 a a a a a bl0
                 a a)) (Split (Oct8 a a a a br0 a
                                    a  a))
    (Split (Oct8 a a
                 a tl1 a a a a)) (Split (Oct8 a  a
                                     tr1 a a a a a))
    (Split (Oct8 a bl1
                 a a a a a a)) (Split (Oct8 br1 a
                                    a  a a a a a))
  where
    a = Fill def


------------------------------------------------------------------------------
-- | Reallocate the bounds of the 'OctTree' so each side length is twice the
-- size.
realloc :: OctTree a -> OctTree a
realloc (OctTree a n q) = OctTree a (n + 1) $ doubleGo a $ unwrap q


------------------------------------------------------------------------------
-- | Get the smallest integer which will contain the 'Cube' when given as an
-- argument to 'mkCubeByPow'.
--
-- @
-- 'cubeContainsCube' ('mkCubeByPow' ('cubeBoundingLog' c)) c == True
-- @
cubeBoundingLog :: Cube Rational -> Integer
cubeBoundingLog (Cube (V3 x y z) (V3 w h d)) =
  maximum $ (0 :) $ fmap (ceiling @Double . logBase 2 . fromRational)
    [ abs x
    , abs $ x + w
    , abs y
    , abs $ y + h
    , abs z
    , abs $ z + d
    ]


fillSel :: (Fractional r, Ord r) => a -> a -> Maybe (Cube r) -> Cube r -> Free a
fillSel def _ Nothing _ = pure def
fillSel def v (Just r) qu = fillImpl def v r qu


fillImpl :: (Fractional r, Ord r) => a -> a -> Cube r -> Cube r -> Free a
fillImpl def v area r
  | cubeContainsCube area r = pure v
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      Split $ fillSel def v <$> subarea <*> subr
  | otherwise = pure def


------------------------------------------------------------------------------
-- | @'cube' def val c@ constructs a new 'OctTree', which has value @val@
-- everywhere in the cube @c@, and @def@ everywhere else.
cube :: a -> a -> Cube Rational -> OctTree a
cube def v (normalize -> r)
  | cubeSize r == 0  = OctTree def (cubeBoundingLog r) $ pure def
  | otherwise = OctTree def (cubeBoundingLog r) $ fillImpl def v r $ mkCubeByPow (cubeBoundingLog r)


------------------------------------------------------------------------------
-- | Fill a 'Cube' with the given value in an 'OctTree'
fill :: forall a. Cube Rational -> a -> OctTree a -> OctTree a
fill (normalize -> r) a q = liftA2 fromMaybe q (cube Nothing (Just a) r)


lookupImpl :: V3 Rational -> Cube Rational -> Free a -> Maybe a
lookupImpl p r ot
  | cubeContainsPoint r p = case ot of
      Fill a -> Just a
      Split qu -> asum $ lookupImpl p <$> subdivide r <*> qu
  | otherwise = Nothing


------------------------------------------------------------------------------
-- | Get the value at the given position in the 'OctTree'.
lookup :: V3 Rational -> OctTree a -> a
lookup v2 (OctTree a n q) = fromMaybe a $ lookupImpl v2 (mkCubeByPow n) q


------------------------------------------------------------------------------
-- | Query a region of space in an 'OctTree'. This method is a special case of
-- 'foldMap', specialized to finite regions.
--
-- For example, if you'd like to check if everything in the 'Cube' has
-- a specific value, use 'Data.Monoid.All' as your choice of 'Semilattice'. If
-- you'd like to check whether anything in the space has a value, instead use
-- 'Data.Monoid.Any'.
query :: Semilattice s => (a -> s) -> Cube Rational -> OctTree a -> s
query f (normalize -> area) (OctTree a n q)
  | cubeContainsCube r area = queryImpl f area r q
  | intersects r area = queryImpl f area r q /\ f a
  | otherwise = f a
  where
    r = mkCubeByPow n


queryImpl :: Semilattice s => (a -> s) -> Cube Rational -> Cube Rational -> Free a -> s
queryImpl f area r (Fill a)
  | intersects area r = f a
  | otherwise = mempty
queryImpl f area r (Split qu)
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      fold $ querySel f <$> subarea <*> subr <*> qu
  | otherwise = mempty


querySel :: Semilattice s => (a -> s) -> Maybe (Cube Rational) -> Cube Rational -> Free a -> s
querySel _ Nothing _ _ = mempty
querySel f (Just area) r q = queryImpl f area r q


------------------------------------------------------------------------------
-- | Partition the 'OctTree' into contiguous, singular-valued 'Cube's.
-- Satsifies the law
--
-- @
-- foldr (uncurry 'fill') (pure $ 'defaultValue' ot) ('toCubes' ot) == ot
-- @
toCubes :: OctTree a -> [(Cube Rational, a)]
toCubes (OctTree _ n q) = toCubesImpl (mkCubeByPow n) q

toCubesImpl :: Cube Rational -> Free a -> [(Cube Rational, a)]
toCubesImpl r (Fill a) = pure (r, a)
toCubesImpl r (Split qu) = do
  let subr = subdivide r
  fold $ toCubesImpl <$> subr <*> qu


------------------------------------------------------------------------------
-- | Get the unique elements contained in the 'OctTree'.
elements :: Ord a => OctTree a -> Set a
elements ot = S.insert (ot_default ot) $ query S.singleton (boundingCube ot) ot


------------------------------------------------------------------------------
-- | Fuse together all adjacent regions of space which contain the same value.
-- This will speed up subsequent queries, but requires traversing the entire
-- tree.
fuse :: Eq a => OctTree a -> OctTree a
fuse (OctTree a n ot) = OctTree a n $ Raw.fuse ot


------------------------------------------------------------------------------
-- | Combine two 'OctTree's using a different semigroup than usual. For
-- example, in order to replace any values in @ot1@ with those covered by
-- @ot2@, we can use:
--
-- @
-- 'combineAla' 'Data.Semigroup.Last' ot1 ot2
-- @
combineAla :: forall n a. (Coercible a n, Semigroup n)  => (a -> n) -> OctTree a -> OctTree a -> OctTree a
combineAla _ x y = coerce $ (coerce x :: OctTree n) <> coerce y

