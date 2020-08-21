{-# LANGUAGE ScopedTypeVariables #-}

module Data.Lsys.Model.Fern
  (
    Alphabet (Branch,TurnLeft,TurnRight,Push,Pop,Constant)
  , grammar
  , grammar2
  ) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as MS
import qualified Data.Sequence as S
import Data.Lsys.Model

data Alphabet = Branch | TurnLeft | TurnRight | Push | Pop | Constant
  deriving (Ord, Eq, Show)


instance CanonicalStr Alphabet where
  canonicalChars Branch = "B"
  canonicalChars Push = "["
  canonicalChars Pop = "]"
  canonicalChars TurnLeft = "+"
  canonicalChars TurnRight = "-"
  canonicalChars Constant = "C"

instance Directionable Alphabet where
  tree lsys =
    let
      translate :: [Alphabet] -> ForwardTurnDirection -> (ForwardTurnDirection, [Alphabet])
      translate [] ftd = (ftd, [])
      translate (Constant:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            translate xs $ ForwardTurnDirection (ftl ++ []) ftdl
      translate (Branch:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            translate xs $ ForwardTurnDirection (ftl ++ [Forward 1]) ftdl
      translate (TurnLeft:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            translate xs $ ForwardTurnDirection (ftl ++ [Turn (-1/22)]) ftdl
      translate (TurnRight:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            translate xs $ ForwardTurnDirection (ftl ++ [Turn (1/22)]) ftdl
      translate (Push:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            let
              (result, remainder) = translate xs $ ForwardTurnDirection [] []
              (popPortion, popRemainder) = translate remainder $ ForwardTurnDirection [] []  
            in 
              (ForwardTurnDirection (ftl)  ([result] ++ [popPortion]), popRemainder)
      translate (Pop:xs) ftd =
        case ftd of
          ForwardTurnDirection ftl ftdl ->
            (ftd,xs)
    in
      let
        (result, _) = translate lsys (ForwardTurnDirection [] [])
      in
        result 

-- F → FXF[-F[-FX]+FX] and X → F++F
grammar :: MS.Map Alphabet [Alphabet]
grammar = MS.fromList
          [(Branch,
            [Branch, Constant, Branch, Push,TurnRight,Branch,Push, TurnRight, Branch, Constant,
            Pop, TurnLeft, Branch, Constant, Pop])
          ,
           (Constant,
            [Branch, TurnLeft, TurnLeft, Branch])
          ,
           (TurnRight,
            [TurnRight])
          ,
           (TurnLeft,
            [TurnLeft])
          ,
           (Push,
            [Push])
          ,
           (Pop,
            [Pop])
          ]

grammar2 :: MS.Map Alphabet [Alphabet]
grammar2 = MS.fromList
           [(Branch, [Branch, TurnLeft, Branch, TurnRight, Branch, TurnRight, Branch, Branch,
                      TurnLeft, Branch, TurnLeft, Branch, TurnRight, Branch])
           ,
            (TurnRight,
             [TurnRight])
           ,
            (TurnLeft,
             [TurnLeft])
           ,
            (Push,
             [Push])
           ,
            (Pop,
             [Pop])
           ,
            (Constant,
             [Constant])
             ]

           
               
          
