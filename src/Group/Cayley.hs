{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Group (
  cayleyTree,
  generate,
  cayleyTable,
  cayleyGraph,
  ) where

import Graph

import Data.List (genericLength, elem, nub, elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Tree as Tree (Tree(..), unfoldTree, unfoldTreeM_BF, flatten)

-- | Generating set and Cayley Tree/Table/Graph
-- a generating set is used to explore the group as a tree, for a given element
-- the left-application of each generator to this element is considered as its
-- own branch
cayleyTree :: (Group g) => [g] -> Tree g
cayleyTree gs = fmap fst $ Tree.unfoldTree f (identity, [])
  where
    gs' = nub gs
    gs'' = map inverse gs' ++ gs'
    f (x, ps)
      | elem x ps = ((x, x : ps), [])
      | otherwise = ((x, x : ps), fmap (\ge -> (x <> ge, x : ps)) gs'')

generate :: (Group g) => [g] -> [g]
generate gs = nub $ Tree.flatten $ cayleyTree gs

cayleyTable :: (Group g) => [g] -> [(g, g, g)]
cayleyTable gs = fmap f $ nub $ Tree.flatten $ h identity (cayleyTree gs)
  where
    f (x, z) = (x, inverse x <> z, z)
    h x (Node z ts) = Node (x, z) (fmap (h z) ts)

cayleyGraph :: (Group g) => [g] -> [(g, Int, [(Int, g)])]
cayleyGraph gs = fmap (\x -> (x, key x, adjacent x)) fgs
  where
    fgs = generate gs
    ct  = cayleyTable gs
    key x = fromJust $ elemIndex x fgs
    adjacent x
      = fmap (\(_, g, x') -> (key x', g))
      $ filter ((==) x . (\(x', g', z') -> x')) ct
