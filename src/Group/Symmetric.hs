{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Symmetric (
  Symmetric, fromCycle
  ) where

import Group

import Data.Proxy
import GHC.TypeLits

-- | (Sn, +) Permutations of n-symbols with composition
newtype Symmetric (n :: Nat)
  = Symmetric [(Integer, Integer)]
  deriving (Eq)

fromCycle :: (KnownNat n) => [Integer] -> Symmetric n
fromCycle cycle = Symmetric p
  where
    order (a, b) = (min a b, max a b)
    p = map (\j -> order (head cycle, j)) (tail cycle)

instance (KnownNat n) => Show (Symmetric n) where
  show (Symmetric p) = concat $ fmap (\(i, j) -> "(" ++ show i ++ " " ++ show j ++ ")") p

instance (KnownNat n) => Group (Symmetric n) where
  identity = Symmetric []
  inverse (Symmetric p) = Symmetric (reverse p)
  operation (Symmetric p) (Symmetric q)
    | null p || null q || last p /= head q = Symmetric (p ++ q)
    | otherwise = operation (Symmetric (init p)) (Symmetric (tail q))
