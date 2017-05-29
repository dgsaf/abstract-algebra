{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Tree (
  Tree,
  pathed,
  depthl,
  breadthl,
  acyclic,

  unfoldTree
  ) where

import Control.Applicative (liftA2)
import Data.Foldable (foldl', foldlM)
import Data.List (sortOn)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Traversable (mapAccumL)

-- tree
data Tree a
  = Tree
  { value :: a
  , succt :: [Tree a]
  }
  deriving (Eq, Read)

-- prettier show instance
pprint :: (Show a) => (Integer -> String) -> Tree a -> String
pprint f t = foldMap (\(v, d) -> f d ++ show v ++ "\n") (depthed t)

instance (Show a) => Show (Tree a) where
  show t = pprint (\d -> concat $ replicate (fromInteger (d - 1)) ".   ") t

-- advanced instances
instance Functor Tree where
  fmap g (Tree v ts) = Tree (g v) (map (fmap g) ts)

instance Applicative Tree where
  pure v = Tree v (repeat (pure v))
  (<*>) (Tree g ts) (Tree v ts') = Tree (g v) (zipWith (<*>) ts ts')

instance Foldable Tree where
  foldMap g (Tree v ts) = (g v) <> mconcat (map (foldMap g) ts)

instance Traversable Tree where
  sequenceA (Tree fv fts) = liftA2 (Tree) (fv) (traverse (sequenceA) fts)

-- depthward propogation
propagate :: (Tree b -> (c, [Tree b])) -> Tree b -> Tree c
propagate f t = Tree w (map (propagate f) ts')
  where
    (w, ts') = f t

prop :: (a -> b -> (a, c)) -> a -> Tree b -> Tree c
prop f a t = Tree w (map (prop f a') (succt t))
  where
    (a', w) = f a (value t)

-- annotations
depthed :: Tree a -> Tree (a, Integer)
depthed t = prop (\n v -> (n + 1, (v, n))) 1 t

pathed :: Tree a -> Tree (a, [a])
pathed t = prop (\p v -> (p ++ [v], (v, p ++ [v]))) [] t

-- transformations
depthl :: Integer -> Tree a -> Tree a
depthl n t = propagate g $ depthed t
  where
    g (Tree (v, d) ts)
      | d < n  = (v, ts)
      | d == n = (v, [])

breadthl :: (Ord b) => (a -> b) -> Integer -> Tree a -> Tree a
breadthl f n t = propagate g t
  where
    g (Tree v ts) = (v, take (fromInteger n) $ sortOn (f . value) ts)

acyclic :: (Eq a) => Tree a -> Tree a
acyclic t = propagate g (pathed t)
  where
    g (Tree (v, ps) ts) = (v, filter (\t -> not $ elem (fst $ value t) ps) ts)

-- constructors
unfoldTree :: (a -> (b, [a])) -> a -> Tree b
unfoldTree f v = Tree w (fmap (unfoldTree f) ts)
  where
    (w, ts) = f v
