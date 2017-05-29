{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Group (
  Group, identity, operation, inverse,
  generate,
  AbelianGroup,
  CountableGroup,
  elements,
  FiniteGroup,
  table
  ) where

import Data.List (genericLength, elem, nub)
import Data.Tree (Tree, unfoldTree)

-- | Group
-- where the following holds for all x, y, z :: g
-- @
-- operation (operation x y) z == operation x (operation y z)
-- operation identity x == operation x identity == x
-- operation x (inverse x) == operation (inverse x) x == identity
-- @
class (Eq g) => Group g where
  identity :: g
  operation :: g -> g -> g
  inverse :: g -> g

-- | Generating set
generate :: (Group g) => [g] -> Tree (g, [g])
generate gs = unfoldTree f (identity, [])
  where
    gs' = nub gs
    f (x, p)
      | elem x p  = ((x, x : p), [])
      | otherwise = ((x, x : p), fmap (\ge -> (operation x ge, x : p)) gs')

-- | Abelian Group
-- where the following holds for all x, y :: g
-- @operation x y = operation y x@
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
table = concat $ fmap (\x -> fmap (\y -> (x, y, operation x y)) xs) xs
  where
    xs = enumFromTo minBound maxBound
