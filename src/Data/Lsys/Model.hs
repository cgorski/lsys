{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


module Data.Lsys.Model
    (
      CanonicalStr
    , canonicalStr
    , canonicalChars
    , root
    , matchFunc
    , calcRoot
    , Directionable
    , tree
    , head
    , DirectionTree (DirectionTree)
    , Direction (Forward, Turn, Push, Pop)
    , symbols 
    ) where

import Control.Monad.Fix

import qualified Data.Set as S
import qualified Data.Map.Strict as MS


data Direction = Forward Float | Turn Float | Push Float | Pop Float deriving Show
data DirectionTree = DirectionTree ([Direction], [DirectionTree]) deriving Show

class CanonicalStr a where
  canonicalChars :: a -> [Char]
  canonicalStr :: [a] -> String
  canonicalStr xs = concatMap canonicalChars xs 

class Directionable a where
  tree :: [a] -> [DirectionTree]

data LSysRoot a = LSysRoot [a] (a -> [a])

instance Show a => Show (LSysRoot a) where
  show (LSysRoot start func) = show start

type LSysList a = [LSysRoot a]

matchFunc :: Ord(a) => MS.Map a [a] -> (a -> [a])
matchFunc matchmap =
  let
    matchFunc sym =
        case (matchmap MS.!? sym) of
          Just xs -> xs
          Nothing -> []
  in
    matchFunc

root :: [a] -> (a -> [a]) -> LSysRoot a
root start subf =
  LSysRoot start subf

rootList :: LSysRoot a -> LSysList a
rootList (LSysRoot start func) =
  let nextgen = LSysRoot (concatMap func start) func in
    (LSysRoot start func):(rootList nextgen)

calcRoot :: LSysRoot a -> Int -> LSysRoot a
calcRoot rt n =
  (rootList rt) !! n

symbols :: LSysRoot a -> Int -> [a]
symbols rt n =
  case calcRoot rt n of
    LSysRoot syms _ -> syms

