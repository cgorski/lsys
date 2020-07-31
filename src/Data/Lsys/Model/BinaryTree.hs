{-# LANGUAGE ScopedTypeVariables #-}

module Data.Lsys.Model.BinaryTree
  (
    Alphabet (Leaf,Branch,PushTurnLeft,PopTurnRight)
  , grammar 
  ) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as MS
import qualified Data.Sequence as S
import Data.Lsys.Model

data Alphabet = Leaf | Branch | PushTurnLeft | PopTurnRight
  deriving (Ord, Eq)

instance Show Alphabet where
  show Leaf = "L"
  show Branch = "B"
  show PushTurnLeft = "["
  show PopTurnRight = "]"

instance Stackable Alphabet where
  stacked lsys =
    let
      push :: [Alphabet] -> [StackedList Alphabet] -> ([StackedList Alphabet], [Alphabet])
      push [] oseq = (oseq, [])
      push (x:[]) oseq = ((StackedList (x, [])):oseq, [])
      push (x:PopTurnRight:xs) oseq = ((StackedList (x, []):oseq), PopTurnRight:xs)
      push (PopTurnRight:xs) oseq =
        let
          (result :: [StackedList Alphabet], (remaining :: [Alphabet])) = push xs []
        in
          push remaining ((StackedList (PopTurnRight, toList result)):oseq)
      push (PushTurnLeft:xs) oseq =
        let
          (result, remaining) = push xs []
        in
          push remaining ((StackedList (PushTurnLeft, toList result)):oseq)
      push (x:xs) oseq = push xs ((StackedList (x, [])):oseq)
      (result, _) = push lsys []
    in
      reverse result

        
grammar :: MS.Map Alphabet [Alphabet]

grammar = MS.fromList
          [(Branch, [Branch, Branch])
          ,(Leaf, [Branch, PushTurnLeft, Leaf, PopTurnRight, Leaf])
          ,(PushTurnLeft, [PushTurnLeft])
          ,(PopTurnRight, [PopTurnRight])
          ]
