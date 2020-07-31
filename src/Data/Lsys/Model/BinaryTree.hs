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
      push :: [Alphabet] -> S.Seq (StackedList Alphabet) -> (S.Seq (StackedList Alphabet), [Alphabet])
      push [] oseq = (oseq, [])
      push (x:[]) oseq = (oseq S.|> StackedList (x, []), [])
      push (x:PopTurnRight:xs) oseq = (oseq S.|> StackedList (x, []), PopTurnRight:xs)
      push (PopTurnRight:xs) oseq =
        let
          (result :: S.Seq (StackedList Alphabet), (remaining :: [Alphabet])) = push xs S.empty
        in
          push remaining (oseq S.|> StackedList (PopTurnRight, toList result))
      push (PushTurnLeft:xs) oseq =
        let
          (result, remaining) = push xs S.empty
        in
          push remaining (oseq S.|> StackedList (PushTurnLeft, toList result))
      push (x:xs) oseq = push xs (oseq S.|> StackedList (x, []))
      (result, _) = push lsys S.empty
    in
      toList result

        
grammar :: MS.Map Alphabet [Alphabet]

grammar = MS.fromList
          [(Branch, [Branch, Branch])
          ,(Leaf, [Branch, PushTurnLeft, Leaf, PopTurnRight, Leaf])
          ,(PushTurnLeft, [PushTurnLeft])
          ,(PopTurnRight, [PopTurnRight])
          ]
