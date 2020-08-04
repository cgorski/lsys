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
      translate :: [Alphabet] -> ForwardTurnDirection -> (ForwardTurnDirection, [Alphabet])
      translate [] ftd = (ftd, [])
      translate (Branch:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            translate xs $ ForwardTurnDirection (ftl ++ [Forward 1]) ftdl
      translate (Leaf:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            translate xs $ ForwardTurnDirection (ftl ++ [Forward (1/2)]) ftdl
      translate (PushTurnLeft:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            let
              (result, remainder) = translate xs $ ForwardTurnDirection [Turn (1/8)] []
              (popPortion, _) = translate remainder $ ForwardTurnDirection [Turn (-1/8)] []  
            in
              (ForwardTurnDirection (ftl)  (ftdl ++ [result] ++ [popPortion]),[])
      translate (PopTurnRight:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            (ftd,xs)
    in
      let
        (result, _) = translate lsys (ForwardTurnDirection [] [])
      in
        result 
                                           

grammar :: MS.Map Alphabet [Alphabet]
grammar = MS.fromList
          [(Branch, [Branch, Branch])
          ,(Leaf, [Branch, PushTurnLeft, Leaf, PopTurnRight, Leaf])
          ,(PushTurnLeft, [PushTurnLeft])
          ,(PopTurnRight, [PopTurnRight])
          ]
