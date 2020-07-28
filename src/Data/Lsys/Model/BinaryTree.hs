module Data.Lsys.Model.BinaryTree
  (
    Alphabet (Leaf,Branch,PushTurnLeft,PopTurnRight)
  , grammar 
  ) where

import qualified Data.Map.Strict as MS

data Alphabet = Leaf | Branch | PushTurnLeft | PopTurnRight
  deriving (Ord, Eq)

instance Show Alphabet where
  show Leaf = "L"
  show Branch = "B"
  show PushTurnLeft = "["
  show PopTurnRight = "]"

grammar :: MS.Map Alphabet [Alphabet]

grammar = MS.fromList
          [(Branch, [Branch, Branch])
          ,(Leaf, [Branch, PushTurnLeft, Leaf, PopTurnRight, Leaf])
          ,(PushTurnLeft, [PushTurnLeft])
          ,(PopTurnRight, [PopTurnRight])
          ]
