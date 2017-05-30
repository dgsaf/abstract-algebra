{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Symmetric (
  Symmetric,
  fromCycles,
  permute,
  display
  ) where

import Group

import Data.List as List
import Data.Map as Map
import Data.Proxy
import Data.Set as Set
import GHC.TypeLits (KnownNat, Nat, natVal)
import Prelude

newtype Symmetric (n :: Nat)
  = Symmetric (Map Integer Integer)
  deriving (Eq, Read, Show)

instance (KnownNat n) => Monoid (Symmetric n) where
  mempty = Symmetric Map.empty
  mappend phi@(Symmetric m) psi@(Symmetric p)
    -- = Symmetric
    -- $ filterWithKey (\i j -> i /= j)
    -- $ Map.union (Map.map (\j -> permute psi j) m) p
    = Symmetric
    $ filterWithKey (\i j -> i /= j)
    $ Map.fromSet (\i -> permute psi (permute phi i))
    $ Set.fromDistinctAscList [1 .. (natVal (Proxy :: Proxy n))]

instance (KnownNat n) => Group (Symmetric n) where
  inverse phi = fromCycles $ fmap (reverse) (toCycles phi)

permute :: Symmetric n -> Integer -> Integer
permute (Symmetric m) i = findWithDefault i i m

toCycles :: Symmetric n -> [[Integer]]
toCycles phi@(Symmetric m)
  = List.filter (not . List.null) $ snd $ mapAccumL
  (\seen i ->
     let ci = toCycle i in
       if elem i seen then
         (seen, [])
       else
         (List.foldl' (flip Set.insert) seen ci, ci))
  Set.empty (keys m)
  where
    toCycle i
      = (:) i $ reverse $ unfoldr
      (\js -> let j = permute phi (head js) in
          if elem j js then
            Nothing
          else Just (j, j : js)) [i]

fromCycles :: [[Integer]] -> Symmetric n
fromCycles cycles
  = Symmetric $ List.foldl' Map.union Map.empty $ fmap fromCycle cycles
  where
    fromCycle cs
      | Prelude.null cs || Prelude.null (tail cs) = Map.empty
      | otherwise = Map.fromList $ zip cs (last cs : cs)

display :: forall n . (KnownNat n) => Symmetric n -> String
display phi
  | List.null cycles = "e"
  | otherwise = concat $ fmap dispCycle cycles
  where
    cycles = toCycles phi
    dispCycle cs =
      "("
      ++ show (head cs)
      ++ concat (fmap (\i -> " " ++ show i) (tail cs))
      ++ ")"
