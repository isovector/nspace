module Data.Semilattice where

import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Product
import qualified Data.HashMap.Monoidal as HashMap
import           Data.Hashable (Hashable)
import qualified Data.IntMap.Monoidal as LazyIntMap
import qualified Data.IntMap.Monoidal.Strict as StrictIntMap
import qualified Data.Map.Monoidal as LazyMap
import qualified Data.Map.Monoidal.Strict as StrictMap
import           Data.Monoid hiding (Product)
import           Data.Semigroup hiding (Product)
import           Data.Set (Set)
import           GHC.Generics

------------------------------------------------------------------------------
-- | A 'Semilattice' is a 'Monoid' with the additional property that its
-- @'(/\)' = '(<>)'@ operation is commutative and idempotent.  That is:
--
-- @
-- a '/\' b = b '/\' a
-- @
--
-- and
--
-- @
-- a '/\' a = a
-- @
--
-- These two properties ensure the internal representations of
-- 'Data.QuadTree.QuadTree' and 'Data.OctTree.OctTree' can't leak out when
-- performing spatial queries.
class Monoid a => Semilattice a where
  (/\) :: a -> a -> a
  (/\) = (<>)

instance Semilattice ()
instance Semilattice Any
instance Semilattice All
instance (Ord a, Bounded a) => Semilattice (Max a)
instance (Ord a, Bounded a) => Semilattice (Min a)
instance Ord a => Semilattice (Set a)
instance (Ord k, Semilattice a) => Semilattice (LazyMap.MonoidalMap k a)
instance (Ord k, Semilattice a) => Semilattice (StrictMap.MonoidalMap k a)
instance Semilattice a => Semilattice (LazyIntMap.MonoidalIntMap a)
instance Semilattice a => Semilattice (StrictIntMap.MonoidalIntMap a)
instance (Hashable k, Eq k, Semilattice a) => Semilattice (HashMap.MonoidalHashMap k a)
instance Semilattice b => Semilattice (a -> b)
instance Semilattice a => Semilattice (Maybe a)
instance Semilattice a => Semilattice (Const a b)
instance Semilattice a => Semilattice (K1 i a b)
instance Semilattice (f a) => Semilattice (M1 i c f a)
instance (Semilattice (f a), Semilattice (g a)) => Semilattice (Product f g a)
instance (Semilattice (f (g a))) => Semilattice (Compose f g a)
instance (Semilattice (f (g a))) => Semilattice ((:.:) f g a)
instance (Semilattice (f a), Semilattice (g a)) => Semilattice ((:*:) f g a)
instance (Semilattice a, Semilattice b) => Semilattice (a, b)
instance (Semilattice a, Semilattice b, Semilattice c) => Semilattice (a, b, c)
instance (Semilattice a, Semilattice b, Semilattice c, Semilattice d) => Semilattice (a, b, c, d)
instance (Semilattice a, Semilattice b, Semilattice c, Semilattice d, Semilattice e) => Semilattice (a, b, c, d, e)

