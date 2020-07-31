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
  deriving (Ord, Eq, Show)


instance CanonicalStr Alphabet where
  canonicalChars Leaf = "L"
  canonicalChars Branch = "B"
  canonicalChars PushTurnLeft = "["
  canonicalChars PopTurnRight = "]"

instance Treeable Alphabet where
  tree lsys =
    let
      push :: [Alphabet] -> [ListTree Alphabet] -> ([ListTree Alphabet], [Alphabet])
      push [] oseq = (oseq, [])
      push (x:[]) oseq = ((ListTree (x, [])):oseq, [])
      push (x:PopTurnRight:xs) oseq = ((ListTree (x, []):oseq), PopTurnRight:xs)
      push (PopTurnRight:xs) oseq =
        let
          (result :: [ListTree Alphabet], (remaining :: [Alphabet])) = push xs []
        in
          push remaining ((ListTree (PopTurnRight, reverse $ toList result)):oseq)
      push (PushTurnLeft:xs) oseq =
        let
          (result, remaining) = push xs []
        in
          push remaining ((ListTree (PushTurnLeft, reverse $ toList result)):oseq)
      push (x:xs) oseq = push xs ((ListTree (x, [])):oseq)
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
