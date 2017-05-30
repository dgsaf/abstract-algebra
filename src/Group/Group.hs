{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Group (
  Group, identity, operation, inverse, (<>),
  generateTree, generateSubgroup, generateTable,
  AbelianGroup,
  CountableGroup,
  elements,
  FiniteGroup,
  table,
  ) where

import Data.List (genericLength, elem, nub, elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Tree as Tree (Tree(..), unfoldTree, flatten)

-- | Group
-- where the following holds for all x, y, z :: g
-- @
-- (x <> y) <> z = x <> (y <> z)
-- identity <> x = x <> identity = x
-- x <> (inverse x) = (inverse x) <> x = identity
-- @
class (Eq g, Monoid g) => Group g where
  identity :: g
  identity = mempty

  operation :: g -> g -> g
  operation = (<>)

  inverse :: g -> g

-- | Generating set
-- a generating set is used to explore the group as a tree
-- tree nodes are of the form (x_k <> g, [x_1 .. x_k]) where x_k are the
-- previously generated elements and g is the generating element used to
-- generate this particular node
generateTree :: (Group g) => [g] -> Tree (g, [g])
generateTree gs = fmap h $ Tree.unfoldTree f (identity, [])
  where
    gs' = nub gs
    f (x, ps)
      | elem x ps = ((x, x : ps), [])
      | otherwise = ((x, x : ps), fmap (\ge -> (x <> ge, x : ps)) gs')
    h (x, p:ps)
      | null ps   = (x, identity : [])
      | otherwise = (x, ps)

generateSubgroup :: (Group g) => [g] -> [g]
generateSubgroup gs = nub $ fmap fst $ Tree.flatten $ generateTree gs

generateTable :: (Group g) => [g] -> [(g, g, g)]
generateTable gs = fmap f $ Tree.flatten $ generateTree gs
  where
    f (x, (y:ys)) = (y, inverse y <> x, x)

    h y (Node x ts) = Node (x, y) (fmap (h x) ts)

-- | Abelian Group
-- where the following holds for all x, y :: g
-- @x <> y = y <> x@
class (Group g) => AbelianGroup g

-- | CountableGroup
-- where the elements of the group are enumerable and where it is assumed that
-- @minBound = identity@
class (Group g, Enum g) => CountableGroup g

elements :: (CountableGroup g) => [g]
elements = enumFrom identity

-- | FiniteGroup
-- where the group has a finite number of elements
class (CountableGroup g, Bounded g) => FiniteGroup g

table :: (FiniteGroup g) => [(g, g, g)]
table = concat $ fmap (\x -> fmap (\y -> (x, y, x <> y)) xs) xs
  where
    xs = enumFromTo minBound maxBound
