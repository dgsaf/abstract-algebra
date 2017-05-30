{-# LANGUAGE DataKinds, KindSignatures #-}
module Main where

import Group
import Cyclic
import Symmetric

import Data.Graph
import Data.Tree

main :: IO ()
main = do
  -- let
  --   i = Cyclic 1 :: Cyclic 4
  --   j = Cyclic 2 :: Cyclic 4

  -- putStrLn $ drawTree $ fmap show $ generate [i, j]
  -- putStrLn $ show $ cayley [i, j]

  let
    phi1 = fromCycles [[1, 2]] :: Symmetric 3
    phi2 = fromCycles [[2, 3]] :: Symmetric 3
    phi3 = fromCycles [[3, 1]] :: Symmetric 3

  putStrLn $ drawTree $ fmap show $ generateTree [phi1, phi2]
  putStrLn $ show $ fmap show $ generateSubgroup [phi1, phi2]

  -- print $ phi1
  -- print $ phi2
  -- print $ phi1 <> phi2
  -- putStrLn $ drawTree $ fmap (show . fst) $ generate [phi1]
  -- putStrLn $ drawTree $ fmap (show . fst) $ generate [phi1, phi2]
  -- putStrLn $ drawTree $ fmap (show . fst) $ generate [phi1, phi2, phi3]
  -- putStrLn $ show $ cayley [phi1, phi2]
