module Space.R3 where

import Data.OctTree


newtype R3 a = R3
  { unR3 :: OctTree a
  }
  deriving newtype (Eq, Show, Functor, Applicative)


