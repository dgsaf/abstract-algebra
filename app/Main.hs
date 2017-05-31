{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Main where

import Group
import Cayley
import Cyclic
import Symmetric

import Data.Tree

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Diagrams.TwoD.Sunburst
-- import Diagrams.TwoD.GraphViz

main :: IO ()
main = visual

draw :: IO ()
draw = mainWith $
  drawSymmetric t1
  ||| drawSymmetric t2
  ||| drawSymmetric t3
  where
    x1 = Cyclic 1 :: Cyclic 4
    x2 = Cyclic 2 :: Cyclic 4
    x3 = Cyclic 3 :: Cyclic 4

    t1 = fmap show $ cayleyTree [x1]
    t2 = fmap show $ cayleyTree [x2]
    t3 = fmap show $ cayleyTree [x3]

    -- w  = (Cyclic 1, Cyclic 2) :: (Cyclic 4, Cyclic 5)
    -- t1 = fmap show $ cayleyTree [w]

drawSunburst :: Tree a -> Diagram B
drawSunburst t = sunburst t # centerXY # pad 1.1

drawSymmetric :: Tree String -> Diagram B
drawSymmetric t = renderTree (\n ->
                text n # fontSizeG 0.5
                <> circle 1.5 # fc white # lw thin)
    (~~) (symmLayout' (with & slHSep .~ 8 & slVSep .~ 8) t)
       # centerXY # pad 1.1

visual :: IO ()
visual = do
  let
    i = Cyclic 1 :: Cyclic 4
    j = Cyclic 2 :: Cyclic 5
    k = Cyclic 2 :: Cyclic 3
    w = (i, j) :: (Cyclic 4, Cyclic 5)
    z = (i, j, k) :: (Cyclic 4, Cyclic 5, Cyclic 3)

  putStrLn $ drawTree $ fmap show $ cayleyTree [i]
  putStrLn $ drawTree $ fmap show $ cayleyTree [j]
  putStrLn $ concat $ fmap (\s -> show s ++ "\n") $ cayleyGraph [w]
  -- putStrLn $ drawTree $ fmap show $ cayleyTree [w]
  -- putStrLn $ show $ fmap show $ generate [w]

  -- let
  --   phi1 = fromCycles [[1, 2]] :: Symmetric 4
  --   phi2 = fromCycles [[2, 3]] :: Symmetric 4
  --   phi3 = fromCycles [[3, 4]] :: Symmetric 4
  --   phi4 = fromCycles [[4, 1]] :: Symmetric 4

  --   ctree = cayleyTree [phi1, phi2]

  -- putStrLn $ drawTree $ fmap Symmetric.display $ cayleyTree [phi1, phi4]
  -- putStrLn $ show $ fmap Symmetric.display $ generate [phi1, phi4]
  -- putStrLn $ concat $ fmap (\s -> show s ++ "\n") $ cayleyTable [phi1, phi4]
  -- putStrLn $ concat
  --   $ fmap (\(x,k,es) -> Symmetric.display x ++ " : " ++ show k ++ " >> " ++ concat (fmap (\(k', g) -> "(" ++ show k' ++ " " ++ Symmetric.display g ++ ") ") es) ++ "\n")
  --   $ cayleyGraph [phi1, phi4]
