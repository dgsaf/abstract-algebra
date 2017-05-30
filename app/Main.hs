{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Main where

import Group
import Cyclic
import Symmetric

import Data.Graph
import Data.Tree

main :: IO ()
main = do
  let
    i = Cyclic 1 :: Cyclic 4
    j = Cyclic 2 :: Cyclic 4

  -- putStrLn $ drawTree $ fmap show $ cayleyTree [i, j]
  -- putStrLn $ show $ fmap show $ generate [i, j]
  -- putStrLn $ concat $ fmap (\s -> show s ++ "\n") $ cayleyTable [i, j]

  let
    phi1 = fromCycles [[1, 2]] :: Symmetric 4
    phi2 = fromCycles [[2, 3]] :: Symmetric 4
    phi3 = fromCycles [[3, 4]] :: Symmetric 4
    phi4 = fromCycles [[4, 1]] :: Symmetric 4

  putStrLn $ drawTree $ fmap Symmetric.display $ cayleyTree [phi1, phi4]
  putStrLn $ show $ fmap Symmetric.display $ generate [phi1, phi4]
  -- putStrLn $ concat $ fmap (\s -> show s ++ "\n") $ cayleyTable [phi1, phi4]
  putStrLn $ concat
    $ fmap (\(x,k,es) -> Symmetric.display x ++ " : " ++ show k ++ " >> " ++ concat (fmap (\(k', g) -> "(" ++ show k' ++ " " ++ Symmetric.display g ++ ") ") es) ++ "\n")
    $ cayleyGraph [phi1, phi4]

  -- print $ phi1
  -- print $ phi2
  -- print $ phi1 <> phi2
  -- putStrLn $ drawTree $ fmap (show . fst) $ generate [phi1]
  -- putStrLn $ drawTree $ fmap (show . fst) $ generate [phi1, phi2]
  -- putStrLn $ drawTree $ fmap (show . fst) $ generate [phi1, phi2, phi3]
  -- putStrLn $ show $ cayley [phi1, phi2]
