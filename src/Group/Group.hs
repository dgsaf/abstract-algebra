{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Group (
  Group, identity, inverse,
  AbelianGroup,
  CountableGroup,
  elements,
  FiniteGroup,
  table,
  ) where

import Data.Monoid ((<>))

-- | Group
-- where the following holds for all x, y, z :: g
-- @
-- (x <> y) <> z = x <> (y <> z)
-- identity <> x = x <> identity = x
-- x <> (inverse x) = (inverse x) <> x = identity
-- @
class (Eq g, Monoid g) => Group g where
  inverse :: g -> g

identity :: (Group g) => g
identity = mempty

-- | Product of Groups
instance (Group g1, Group g2) => Group (g1, g2) where
  inverse (x1, x2) = (inverse x1, inverse x2)

-- | Abelian Group
-- where the following holds for all x, y :: g
-- @x <> y = y <> x@
class (Group g) => AbelianGroup g

-- | Countable Group
-- where the elements of the group are enumerable and where it is assumed that
-- @minBound = identity@
class (Group g, Enum g) => CountableGroup g

-- | Enumerate all elements of a countable group
elements :: (CountableGroup g) => [g]
elements = enumFrom identity

-- | Direct construction of Cayley table by enumeration
table :: (CountableGroup g) => [(g, g, g)]
table = concat $ fmap (\x -> fmap (\y -> (x, y, x <> y)) elements) elements

-- | Finite Group
-- where the group has a finite number of elements
class (CountableGroup g, Bounded g) => FiniteGroup g
