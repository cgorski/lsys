{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables       #-}


module Main where

import Data.Lsys.Model as M
import qualified Data.Lsys.Model.Algae as A
import qualified Data.Lsys.Model.BinaryTree as BT
import qualified Data.Lsys.Model.Fern as F
import Data.Foldable

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

import qualified Data.Sequence as S

data ConversionState a b v n = ConversionState
                       { nextFunc :: a -> b,
                         nextScaleFactor :: Double,
                         nextVector :: v n,
                         vectors :: S.Seq (v n)
                       }

diagramOfDirections :: ForwardTurnDirection -> Diagram B
diagramOfDirections dirs =
  let
    startState = ConversionState  {
      nextFunc = id,
      nextScaleFactor = 1,
      nextVector = unitY,
      vectors = S.empty
      }
    
    subdf :: ForwardTurnDirection -> ConversionState (V2 Double) (V2 Double) V2 Double -> Point V2 Double -> Path V2 Double
    subdf (ForwardTurnDirection dirs subdirs) state startPoint =
      let
        (trail, lastPoint, lastState) = df dirs state []
        path = pathFromTrailAt trail startPoint
        newStartPoint = startPoint + (p2 lastPoint)
        newStartState = ConversionState {
          nextFunc = nextFunc state,
          nextScaleFactor = nextScaleFactor state,
          nextVector = nextVector lastState,
          vectors = vectors state
          }
      in
        mconcat (path:(map (\sub -> subdf sub newStartState newStartPoint) subdirs))

    df :: [ForwardTurn] -> ConversionState (V2 Double) (V2 Double) V2 Double -> [Int] -> (Trail V2 Double, (Double, Double), ConversionState (V2 Double) (V2 Double) V2 Double)
    df [] state name =
      let
        vecList = toList (vectors state)
        trail = vecList # trailFromOffsets
        lastPoint ::  (Double, Double)
        lastPoint = unr2 $ foldl (^+^) (r2 (0.0,0.0)) vecList
      in
        (trail, lastPoint, state)
    df (x:xs) state name = 
      case x of 
        Forward dirScale ->
          let
            funcAppliedVec = (nextVector state) # nextFunc state
            nextState = ConversionState {
              nextFunc = id,
              nextScaleFactor = nextScaleFactor state,
              nextVector = funcAppliedVec,
              vectors = (vectors state S.|> funcAppliedVec # scale ((nextScaleFactor state)*dirScale))
              }
          in
            df xs nextState name
        Turn dirAngle ->
          let
            nextState = ConversionState {
              nextFunc = (nextFunc state) . (rotateBy dirAngle), 
              nextScaleFactor = nextScaleFactor state,
              nextVector = nextVector state,
              vectors = vectors state
              }
          in
            df xs nextState name
  in
    subdf dirs startState (p2 (0.0,0.0)) # strokePath # lc green # lw 2




 
recurse :: M.ForwardTurnDirection
recurse = ForwardTurnDirection [Forward (2/8), Turn (1/8), Forward 1, Forward (1/8), Turn (-2/8), Forward 1]
          [ForwardTurnDirection [Forward 2] [], ForwardTurnDirection [Turn (1/8), Forward 1] []]

manual = ForwardTurnDirection [Forward 1.0,Forward 1.0] [ForwardTurnDirection [Turn 0.125,Forward 1.0] [ForwardTurnDirection [Turn 0.125,Forward 0.5] [],ForwardTurnDirection [Turn (-0.125),Forward 0.5] []],ForwardTurnDirection [Turn (-0.125)] []]

manual2 =
  ForwardTurnDirection [Forward 1.0] [
  ForwardTurnDirection [Turn (-0.125),Forward 1.0] [
      ForwardTurnDirection [Turn (-0.125),Forward 1.0] []], ForwardTurnDirection [Turn (0.125),Forward 1.0] []]


main :: IO ()
main = 
  let
    algaeroot = root [A.A] (matchFunc A.grammar) 
    treeroot = root [BT.Leaf] (matchFunc BT.grammar)
    fernroot = root [F.Constant] (matchFunc F.grammar)
  in
    do
--      renderSVG "temp/circle.svg" (mkSizeSpec2D (Just 800) (Just 800)) $ branch3
--      renderSVG "temp/circle.svg" (mkSizeSpec2D (Just 800) (Just 800)) $ diagramOfDirections recurse
--      renderSVG "temp/circle.svg" (mkSizeSpec2D (Just 800) (Just 800)) (tree1 # (withName "foo" $ \d -> tree2))
      putStrLn $ canonicalStr $ symbols treeroot 0
      putStrLn $ canonicalStr $ symbols treeroot 1
      putStrLn $ canonicalStr $ symbols treeroot 2

      putStrLn $ canonicalStr $ symbols algaeroot 0
      putStrLn $ canonicalStr $ symbols algaeroot 1
      putStrLn $ canonicalStr $ symbols algaeroot 2

      putStrLn ""
      putStrLn ""
      putStrLn $ canonicalStr $ symbols fernroot 0
      putStrLn ""
      putStrLn $ canonicalStr $ symbols fernroot 1
      putStrLn ""
      putStrLn $ canonicalStr $ symbols fernroot 2
      putStrLn ""
      putStrLn $ canonicalStr $ symbols fernroot 3

      

--      putStrLn $ show $ tree $ symbols treeroot 7
      renderSVG "temp/circle.svg" (mkSizeSpec2D (Just 800) (Just 800)) $ diagramOfDirections $ tree $ symbols treeroot 7
      renderSVG "temp/recurse.svg" (mkSizeSpec2D (Just 800) (Just 800)) $ diagramOfDirections recurse
      renderSVG "temp/manual.svg" (mkSizeSpec2D (Just 800) (Just 800)) $ diagramOfDirections manual
      renderSVG "temp/manual2.svg" (mkSizeSpec2D (Just 800) (Just 800)) $ diagramOfDirections manual2      
--      putStrLn $ show $ tree $ symbols treeroot 0
--      putStrLn $ show $ tree $ symbols treeroot 1
--      putStrLn $ show $ tree $ symbols treeroot 2
  
 
