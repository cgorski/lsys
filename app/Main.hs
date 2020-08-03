{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


module Main where

import Data.Lsys.Model as M
import qualified Data.Lsys.Model.Algae as A
import qualified Data.Lsys.Model.BinaryTree as BT
import Data.Foldable

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

import qualified Data.Sequence as S

-- diagram :: Diagram SVG R2

myCircle :: Diagram B
myCircle = circle 1

line :: Diagram B
line = strokeT . fromOffsets $ [unitY]

line2 :: Diagram B
line2 = strokeT . fromOffsets $ [unitX # rotateBy (-(1/5)) ]

line3 :: Diagram B
line3 = atPoints (map p2 [(0,0), (0,1)]) (repeat (circle 0.2 # fc green))

line4 :: Diagram B
line4 = atPoints (map p2 [(0,0), (0,1)]) [line, line2]

tree1 :: Diagram B
tree1 = (fromOffsets $ [unitX, unitX # rotateBy ((1/8)) # scale (1/2)]) # named "foo"

tree2 :: Diagram B
tree2 = (fromOffsets $ [unitY]) # lc green # named "bar"

branch1  :: Diagram B
branch1 = [p2 (0,0), p2 (0,1)] # fromVertices # strokeTrail # lc green # lw 2

branch2 :: Diagram B
branch2 = [unitY, unitY # rotateBy (1/8)] # fromOffsets # fromVertices # strokeTrail # lc green # lw 2

branch3 :: Diagram B
branch3 = hrule 1 # lc green # lw 2 <> hrule (1/2) # rotateBy (1/8) <> hrule 2


  -- let
  --   a = S.fromList []
  --   a1 = a S.|> unitY
  --   a2 = 
  --   a2 = a1 S.|> unitY
    
recurse :: [M.ForwardTurnDirection]
recurse = [Forward (1/8), Turn (1/8), Forward 1, Forward (1/8), Turn (-2/8), Forward 1]

data ConversionState a b v n = ConversionState
                       { nextFunc :: a -> b,
                         nextScaleFactor :: Double,
                         nextVector :: v n,
                         vectors :: S.Seq (v n),
                         diagrams :: S.Seq (Diagram B)
                       }

diagramOfDirections :: [ForwardTurnDirection] -> Diagram B
diagramOfDirections dirs =
  let
    df [] state =
      (toList (vectors state)) # fromOffsets # fromVertices # strokeTrail # lc green # lw 2
    df (x:xs) state = 
      case x of 
        Forward dirScale ->
          let
            funcAppliedVec = (nextVector state) # nextFunc state
            nextState = ConversionState {
              nextFunc = id,
              nextScaleFactor = 1,
              nextVector = funcAppliedVec,
              vectors = (vectors state S.|> funcAppliedVec # scale ((nextScaleFactor state)*dirScale)),
              diagrams = S.empty
              }
          in
            df xs nextState
        Turn dirAngle ->
          let
            nextState = ConversionState {
              nextFunc = (nextFunc state) . (rotateBy dirAngle), 
              nextScaleFactor = 1,
              nextVector = nextVector state,
              vectors = vectors state,
              diagrams = S.empty
              }
          in
            df xs nextState
        SubDirections sub -> df xs state
  in
    let
      state = ConversionState  {
        nextFunc = id,
        nextScaleFactor = 1,
        nextVector = unitX,
        vectors = S.empty, 
        diagrams = S.empty
      }
    in
      df dirs state 

-- diagramOfDirections2 :: [ForwardTurnDirection] -> Diagram B
-- diagramOfDirections2 dirs =
--   let
-- --    df :: [ForwardTurnDirection] -> (a -> b) -> S.Seq(V2 n) -> [Diagram B] -> Diagram B
--     df [] _ ovec odiag = (toList ovec) # fromOffsets # fromVertices # strokeTrail # lc green # lw 2
--     df (x:xs) tranfunc vec ovec odiag =
--       case x of 
--         Forward s ->
--           df xs id (ovec S.|> unitX # scale s # tranfunc) odiag
--         Turn a ->
--           df xs (rotateBy a . tranfunc) ovec odiag
--         SubDirections sub -> df [] tranfunc ovec odiag
--   in
--     df dirs id unitY S.empty []
 

main :: IO ()
main = 
  let
    algaeroot = root [A.A] (matchFunc A.grammar) 
    treeroot = root [BT.Leaf] (matchFunc BT.grammar)
  in
    do
--      renderSVG "temp/circle.svg" (mkSizeSpec2D (Just 800) (Just 800)) $ branch3
      renderSVG "temp/circle.svg" (mkSizeSpec2D (Just 800) (Just 800)) $ diagramOfDirections recurse
--      renderSVG "temp/circle.svg" (mkSizeSpec2D (Just 800) (Just 800)) (tree1 # (withName "foo" $ \d -> tree2))
      putStrLn $ canonicalStr $ symbols treeroot 0
      putStrLn $ canonicalStr $ symbols treeroot 1
      putStrLn $ canonicalStr $ symbols treeroot 2

      putStrLn $ canonicalStr $ symbols algaeroot 0
      putStrLn $ canonicalStr $ symbols algaeroot 1
      putStrLn $ canonicalStr $ symbols algaeroot 2

--      putStrLn $ show $ tree $ symbols treeroot 0
--      putStrLn $ show $ tree $ symbols treeroot 1
--      putStrLn $ show $ tree $ symbols treeroot 2
  
 
