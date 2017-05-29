{-# LANGUAGE DataKinds, KindSignatures #-}
module Main where

import Group
import Cyclic
import Symmetric

import Data.Tree

main :: IO ()
main = do
  let
    m = Cyclic 1 :: Cyclic 4
    m' = Cyclic 3 :: Cyclic 4
    gtree = generate [m, m']
  putStrLn $ drawTree $ fmap show $ gtree

  let
    phi = fromCycle [3,4,1] :: Symmetric 4
    psi = fromCycle [1,3,4,2] :: Symmetric 4
  putStrLn $ show phi
  putStrLn $ show psi
  putStrLn $ show $ operation phi psi
  putStrLn $ show $ inverse phi
  putStrLn $ show $ operation phi (inverse phi)
