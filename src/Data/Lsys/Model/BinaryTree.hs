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

-- instance Directionable Alphabet where
--   tree lsys =
--     let
--       translate :: Alphabet -> [Direction]
--       translate Branch = [Forward 1]
--       translate Leaf = [Forward (1/2)]
--       translate PushTurnLeft = [Push (1/2), Turn ((-1)/2)]
--       translate PopTurnRight = [Pop (1/2), Turn (1/2)]

-- --      merge :: [DirectionTree] -> [DirectionTree]
-- --      merge
      
--       push :: [Alphabet] -> [DirectionTree] -> ([DirectionTree], [Alphabet])
--       push [] oseq = (oseq, [])
--       push (x:[]) oseq = ((DirectionTree (translate x, [])):oseq, [])
--       push (x:PopTurnRight:xs) oseq = ((DirectionTree (translate x, []):oseq), PopTurnRight:xs)
--       push (PopTurnRight:xs) oseq =
--         let
--           (result :: [DirectionTree], (remaining :: [Alphabet])) = push xs []
--         in
--           push remaining ((DirectionTree (translate PopTurnRight, reverse $ toList result)):oseq)
--       push (PushTurnLeft:xs) oseq =
--         let
--           (result, remaining) = push xs []
--         in
--           push remaining ((DirectionTree (translate PushTurnLeft, reverse $ toList result)):oseq)
--       push (x:xs) oseq = push xs ((DirectionTree (translate x, [])):oseq)
--       (result, _) = push lsys []
--     in
--       reverse result

        
grammar :: MS.Map Alphabet [Alphabet]

grammar = MS.fromList
          [(Branch, [Branch, Branch])
          ,(Leaf, [Branch, PushTurnLeft, Leaf, PopTurnRight, Leaf])
          ,(PushTurnLeft, [PushTurnLeft])
          ,(PopTurnRight, [PopTurnRight])
          ]
