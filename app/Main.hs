{-# LANGUAGE DataKinds, KindSignatures #-}
module Main where

import Group

import Data.Tree

main :: IO ()
main = do
  let
    m = Z 1 :: Z 4
    m' = Z 3 :: Z 4
    gtree = generate [m, m']

  putStrLn $ drawTree $ fmap show $ gtree
  -- putStrLn $ take 100 $ drawTree $ fmap show $ cayley [m, m']
