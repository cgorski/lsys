module Main where

import Data.Lsys.Core
import qualified Data.Lsys.Model.Algae as A
import qualified Data.Lsys.Model.BinaryTree as BT

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

-- diagram :: Diagram SVG R2

myCircle :: Diagram B
myCircle = circle 1

line :: Diagram B
line = strokeT . fromOffsets $ [unitY]

line2 :: Diagram B
line2 = strokeT . fromOffsets $ [unitY # rotateBy (-(1/16)) ]

line3 :: Diagram B
line3 = atPoints (map p2 [(0,0), (0,1)]) (repeat (circle 0.2 # fc green))

line4 :: Diagram B
line4 = atPoints (map p2 [(0,0), (0,1)]) [line, line2]

main :: IO ()
main = 
  let
    algaeroot = createRoot [A.A] (createMatchFunc A.grammar)
    treeroot = createRoot [BT.Leaf] (createMatchFunc BT.grammar)
  in
    do
      renderSVG "circle.svg" (mkSizeSpec2D (Just 800) (Just 800)) line4
      putStrLn $ show $ genSymbols treeroot 0
      putStrLn $ show $ genSymbols treeroot 1
      putStrLn $ show $ genSymbols treeroot 2

      putStrLn $ show $ genSymbols algaeroot 0
      putStrLn $ show $ genSymbols algaeroot 1
      putStrLn $ show $ genSymbols algaeroot 2 
  
 
