{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


module Data.Lsys.Core
    (
      canonicalStr 
    , createRoot
    , createMatchFunc
    , genRoots
    , genSymbols 
    ) where

import Control.Monad.Fix

import qualified Data.Set as S
import qualified Data.Map.Strict as MS

data LSysRoot a = LSysRoot [a] (a -> [a])

instance Show a => Show (LSysRoot a) where
  show (LSysRoot start func) = show start

type LSysList a = [LSysRoot a]

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

genRoots :: LSysRoot a -> Int -> LSysRoot a
genRoots root n =
  (createList root) !! n

genSymbols :: LSysRoot a -> Int -> [a]
genSymbols root n =
  case genRoots root n of
    LSysRoot symbols _ -> symbols

canonicalStr :: Show a => [a] -> String
canonicalStr xs =
  concatMap show xs 
