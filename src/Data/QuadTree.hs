{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData      #-}

module Data.QuadTree
  ( QuadTree (..)

    -- * Constructing 'QuadTree's
  , rect
  , fill
  , combineAla

    -- * Spatially Querying 'QuadTree's
  , lookup
  , query

    -- * Eliminating 'QuadTree's
  , fuse
  , elements
  , toRects
  , boundingRect
  , defaultValue

    -- * Constructing 'Rect's
  , Rect (..)
  , mkRectByPow

    -- * Eliminating 'Rect's
  , midpoint
  , subdivide
  , Raw.rectCorners

    -- * Indexing Types
  , V2 (..)
  , V4 (..)
  ) where

import           Data.Coerce
import           Data.Foldable
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Ap(..))
import           Data.QuadTree.Internal (Free(..), Rect(..), unwrap, intersects, rectContainsRect, getIntersect, rectContainsPoint, normalize, rectSize)
import qualified Data.QuadTree.Internal as Raw
import           Data.Semilattice
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC.Base (liftA2)
import           Linear.V2
import           Linear.V4
import           Prelude hiding (lookup)


------------------------------------------------------------------------------
-- | Compute the center of a 'Rect'.
midpoint :: (Fractional a) => Rect a -> V2 a
midpoint (Rect pos sz) = pos + sz / 2


------------------------------------------------------------------------------
-- | Subdivide a 'Rect' into four 'Rect's which fill up the same volume.
subdivide :: Fractional a => Rect a -> V4 (Rect a)
subdivide (Rect (V2 x y) (V2 w h)) =
  let halfw = w / 2
      halfh = h / 2
   in V4
        (Rect (V2 x y) (V2 halfw halfh))
        (Rect (V2 (x + halfw) y) $ V2 (w - halfw) halfh)
        (Rect (V2 x (y + halfh)) $ V2 halfw (h - halfh))
        (Rect (V2 (x + halfw) (y + halfh)) $ V2 (w - halfw) (h - halfh))


------------------------------------------------------------------------------
-- | A type mapping values at (infinitely precise) locations in 2D
-- space. That is, you can consider an 'QuadTree' to be a function @'V2'
-- 'Rational' -> a@, equipped with efficient means of querying the space.
--
-- 'QuadTree's should usually be constructed using their 'Monoid'al or
-- 'Applicative' interfaces, as well as by way of the 'rect' and 'fill'
-- functions.
data QuadTree a = QuadTree
  { ot_default  :: a
  , ot_root_pow :: Integer
  , ot_tree     :: Free a
  }
  deriving stock (Show, Functor)
  deriving (Num, Semigroup, Monoid) via (Ap QuadTree a)

instance Semilattice a => Semilattice (QuadTree a)


------------------------------------------------------------------------------
-- | Get the value used to fill the infinity of space in an 'QuadTree'.
defaultValue :: QuadTree a -> a
defaultValue = ot_default

instance Eq a => Eq (QuadTree a) where
  q1@(QuadTree a m tr) == q2@(QuadTree a' n tr') =
    case compare m n of
      LT -> realloc q1 == q2
      EQ -> a == a' && tr == tr'
      GT -> q1 == realloc q2


instance Applicative QuadTree where
  pure a = QuadTree a 0 $ pure a
  liftA2 fabc q1@(QuadTree a m ota) q2@(QuadTree b n otb) =
    case compare m n of
      LT -> liftA2 fabc (realloc q1) q2
      EQ -> QuadTree (fabc a b) m $ liftA2 fabc ota otb
      GT -> liftA2 fabc q1 (realloc q2)


------------------------------------------------------------------------------
-- | Get a 'Rect' guaranteed to bound all of the non-defaulted values in the
-- 'QuadTree'.
boundingRect :: QuadTree a -> Rect Rational
boundingRect = mkRectByPow . ot_root_pow


------------------------------------------------------------------------------
-- | Construct a 'Rect' centered around $(0, 0, 0)$, with side length $2n$.
mkRectByPow :: Integer -> Rect Rational
mkRectByPow n =
  let side = 2 ^ n
   in Rect (pure (-side)) $ pure $ side * 2


------------------------------------------------------------------------------
-- | Build a larger 'Free' 'Quad' by doubling each side length, keeping the
-- contents in the center.
doubleGo :: a -> V4 (Free a) -> Free a
doubleGo def (V4 tl tr bl br) = Split $
  V4
    (Split (V4 a a
               a tl)) (Split (V4 a  a
                                tr a))
    (Split (V4 a bl
               a a)) (Split (V4 br a
                                a  a))
  where
    a = Fill def


------------------------------------------------------------------------------
-- | Reallocate the bounds of the 'QuadTree' so each side length is twice the
-- size.
realloc :: QuadTree a -> QuadTree a
realloc (QuadTree a n q) = QuadTree a (n + 1) $ doubleGo a $ unwrap q


------------------------------------------------------------------------------
-- | Get the smallest integer which will contain the 'Rect' when given as an
-- argument to 'mkRectByPow'.
--
-- @
-- 'rectContainsRect' ('mkRectByPow' ('rectBoundingLog' c)) c == True
-- @
rectBoundingLog :: Rect Rational -> Integer
rectBoundingLog (Rect (V2 x y) (V2 w h)) =
  maximum $ (0 :) $ fmap (ceiling @Double . logBase 2 . fromRational)
    [ abs x
    , abs $ x + w
    , abs y
    , abs $ y + h
    ]


fillSel :: (Fractional r, Ord r) => a -> a -> Maybe (Rect r) -> Rect r -> Free a
fillSel def _ Nothing _ = pure def
fillSel def v (Just r) qu = fillImpl def v r qu


fillImpl :: (Fractional r, Ord r) => a -> a -> Rect r -> Rect r -> Free a
fillImpl def v area r
  | rectContainsRect area r = pure v
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      Split $ fillSel def v <$> subarea <*> subr
  | otherwise = pure def


------------------------------------------------------------------------------
-- | @'rect' def val c@ constructs a new 'QuadTree', which has value @val@
-- everywhere in the rect @c@, and @def@ everywhere else.
rect :: a -> a -> Rect Rational -> QuadTree a
rect def v (normalize -> r)
  | rectSize r == 0  = QuadTree def (rectBoundingLog r) $ pure def
  | otherwise = QuadTree def (rectBoundingLog r) $ fillImpl def v r $ mkRectByPow (rectBoundingLog r)


------------------------------------------------------------------------------
-- | Fill a 'Rect' with the given value in an 'QuadTree'
fill :: forall a. Rect Rational -> a -> QuadTree a -> QuadTree a
fill (normalize -> r) a q = liftA2 fromMaybe q (rect Nothing (Just a) r)


lookupImpl :: V2 Rational -> Rect Rational -> Free a -> Maybe a
lookupImpl p r ot
  | rectContainsPoint r p = case ot of
      Fill a -> Just a
      Split qu -> asum $ lookupImpl p <$> subdivide r <*> qu
  | otherwise = Nothing


------------------------------------------------------------------------------
-- | Get the value at the given position in the 'QuadTree'.
lookup :: V2 Rational -> QuadTree a -> a
lookup v2 (QuadTree a n q) = fromMaybe a $ lookupImpl v2 (mkRectByPow n) q


------------------------------------------------------------------------------
-- | Query a region of space in an 'QuadTree'. This method is a special case of
-- 'foldMap', specialized to finite regions.
--
-- For example, if you'd like to check if everything in the 'Rect' has
-- a specific value, use 'Data.Monoid.All' as your choice of 'Semilattice'. If
-- you'd like to check whether anything in the space has a value, instead use
-- 'Data.Monoid.Any'.
query :: Semilattice s => (a -> s) -> Rect Rational -> QuadTree a -> s
query f (normalize -> area) (QuadTree a n q)
  | rectContainsRect r area = queryImpl f area r q
  | intersects r area = queryImpl f area r q /\ f a
  | otherwise = f a
  where
    r = mkRectByPow n


queryImpl :: Semilattice s => (a -> s) -> Rect Rational -> Rect Rational -> Free a -> s
queryImpl f area r (Fill a)
  | intersects area r = f a
  | otherwise = mempty
queryImpl f area r (Split qu)
  | intersects area r = do
      let subr = subdivide r
          subarea = getIntersect area <$> subr
      fold $ querySel f <$> subarea <*> subr <*> qu
  | otherwise = mempty


querySel :: Semilattice s => (a -> s) -> Maybe (Rect Rational) -> Rect Rational -> Free a -> s
querySel _ Nothing _ _ = mempty
querySel f (Just area) r q = queryImpl f area r q


------------------------------------------------------------------------------
-- | Partition the 'QuadTree' into contiguous, singular-valued 'Rect's.
-- Satsifies the law
--
-- @
-- 'foldMap' (uncurry $ 'rect' ('defaultValue' ot)) ('toRects' ot) == ot
-- @
toRects :: QuadTree a -> [(Rect Rational, a)]
toRects (QuadTree _ n q) = toRectsImpl (mkRectByPow n) q

toRectsImpl :: Rect Rational -> Free a -> [(Rect Rational, a)]
toRectsImpl r (Fill a) = pure (r, a)
toRectsImpl r (Split qu) = do
  let subr = subdivide r
  fold $ toRectsImpl <$> subr <*> qu


------------------------------------------------------------------------------
-- | Get the unique elements contained in the 'QuadTree'.
elements :: Ord a => QuadTree a -> Set a
elements ot = S.insert (ot_default ot) $ query S.singleton (boundingRect ot) ot


------------------------------------------------------------------------------
-- | Fuse together all adjacent regions of space which contain the same value.
-- This will speed up subsequent queries, but requires traversing the entire
-- tree.
fuse :: Eq a => QuadTree a -> QuadTree a
fuse (QuadTree a n ot) = QuadTree a n $ Raw.fuse ot


------------------------------------------------------------------------------
-- | Combine two 'QuadTree's using a different semigroup than usual. For
-- example, in order to replace any values in @qt1@ with those covered by
-- @qt2@, we can use:
--
-- @
-- 'combineAla' 'Data.Semigroup.Last' qt1 qt2
-- @
combineAla :: forall n a. (Coercible a n, Semigroup n)  => (a -> n) -> QuadTree a -> QuadTree a -> QuadTree a
combineAla _ x y = coerce $ (coerce x :: QuadTree n) <> coerce y


