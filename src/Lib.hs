{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


module Lib
    ( lsys
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
-- import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Size


import Control.Exception.Safe
import Control.Monad.Fix

import qualified Data.Set as S
import qualified Data.Map.Strict as MS

data MyException = MyException
  deriving (Show, Typeable)
instance Exception MyException

data LSysRoot a = LSysRoot [a] (a -> [a])
instance Show (LSysRoot Char) where
  show (LSysRoot start func) = show start

type LSysList a = [LSysRoot a]

isSubset :: Ord a => [a] -> [a] -> Bool
isSubset xs ys = all (`S.member` yset) xs where
  yset = S.fromList ys

algae :: Char -> [Char]
algae 'A' = ['A','B']
algae 'B' = ['B','A']
algae _ = []

tree :: Char -> [Char]
tree '0' = ['1','[','0',']','0']
tree '1' = ['1','1']
tree '[' = ['[']
tree ']' = [']']

tree2 = MS.fromList [('0',['1', '[','0',']','0'])
        ,('1',['1', '1'])
        ,('[',['['])
        ,(']',[']'])]

createMatchFunc :: Ord(a) => MS.Map a [a] -> (a -> [a])
createMatchFunc matchmap =
  let
    matchFunc sym =
        case (matchmap MS.!? sym) of
          Just xs -> xs
          Nothing -> []
  in
    matchFunc


createRoot :: [a] -> (a -> [a]) -> LSysRoot a
createRoot start subf =
  LSysRoot start subf

createList :: LSysRoot a -> LSysList a
createList (LSysRoot start func) =
  let nextgen = LSysRoot (concatMap func start) func in
    (LSysRoot start func):(createList nextgen)

genRoot :: LSysRoot a -> Int -> LSysRoot a
genRoot root n =
  (createList root) !! n

genSymbols :: LSysRoot a -> Int -> [a]
genSymbols root n =
  case genRoot root n of
    LSysRoot symbols _ -> symbols

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


lsys :: IO ()
lsys =
  let
    algaeroot = createRoot ['A'] algae
    algaesym = genSymbols algaeroot 6
    treeroot = createRoot ['0'] tree
    tree2root = createRoot ['0'] (createMatchFunc tree2)
    
  in
    do
      renderSVG "circle.svg" (mkSizeSpec2D (Just 800) (Just 800)) line4
      putStrLn "foo"
      putStrLn $ show algaesym
      putStrLn $ show $ genSymbols tree2root 0
      putStrLn $ show $ genSymbols tree2root 1
      putStrLn $ show $ genSymbols tree2root 2

