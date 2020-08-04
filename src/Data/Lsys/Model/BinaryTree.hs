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

instance Directionable Alphabet where
  tree lsys =
    let
      translate :: [Alphabet] -> ForwardTurnDirection -> ForwardTurnDirection
      translate [] ftd = ftd
      translate (Branch:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            translate xs $ ForwardTurnDirection (ftl ++ [Forward 1]) []
      translate (x:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            translate xs $ ForwardTurnDirection (ftl ++ [Forward (1/2)]) []
    in
      translate lsys (ForwardTurnDirection [] [])
                                           

grammar :: MS.Map Alphabet [Alphabet]
grammar = MS.fromList
          [(Branch, [Branch, Branch])
          ,(Leaf, [Branch, PushTurnLeft, Leaf, PopTurnRight, Leaf])
          ,(PushTurnLeft, [PushTurnLeft])
          ,(PopTurnRight, [PopTurnRight])
          ]
