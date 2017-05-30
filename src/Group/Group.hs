{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Group (
  Group, identity, operation, inverse,
  cayleyTree, cayleyTable, generate, cayleyGraph,
  AbelianGroup,
  CountableGroup,
  elements,
  FiniteGroup,
  table,
  ) where

import Data.List (genericLength, elem, nub, elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Tree as Tree (Tree(..), unfoldTree, unfoldTreeM_BF, flatten)

-- | Group
-- where the following holds for all x, y, z :: g
-- @
-- (x <> y) <> z = x <> (y <> z)
-- identity <> x = x <> identity = x
-- x <> (inverse x) = (inverse x) <> x = identity
-- @
class (Eq g, Monoid g) => Group g where
  inverse :: g -> g

  identity :: g
  identity = mempty

  operation :: g -> g -> g
  operation = (<>)

-- | Generating set
-- a generating set is used to explore the group as a tree, for a given element
-- the left-application of each generator to this element is considered as its
-- own branch
cayleyTree :: (Group g) => [g] -> Tree g
cayleyTree gs = fmap fst $ Tree.unfoldTree f (identity, [])
  where
    gs' = nub gs
    f (x, ps)
      | elem x ps = ((x, x : ps), [])
      | otherwise = ((x, x : ps), fmap (\ge -> (x <> ge, x : ps)) gs')

generate :: (Group g) => [g] -> [g]
generate gs = nub $ Tree.flatten $ cayleyTree gs

cayleyTable :: (Group g) => [g] -> [(g, g, g)]
cayleyTable gs = fmap f $ nub $ Tree.flatten $ h identity (cayleyTree gs)
  where
    f (x, y) = (y, inverse y <> x, x)
    h y (Node x ts) = Node (x, y) (fmap (h x) ts)

cayleyGraph :: (Group g) => [g] -> [(g, Int, [(Int, g)])]
cayleyGraph gs = fmap (\x -> (x, key x, adjacent x)) fgs
  where
    fgs = generate gs
    ct  = cayleyTable gs
    key x = fromJust $ elemIndex x fgs
    adjacent x
      = fmap (\(_, g, x') -> (key x', g))
      $ filter ((==) x . (\(a, b, c) -> a)) ct

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
