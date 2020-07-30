{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


module Data.Lsys.Core
    (
      canonicalStr 
    , root
    , matchFunc
    , calcRoot
    , symbols 
    ) where

import Control.Monad.Fix

import qualified Data.Set as S
import qualified Data.Map.Strict as MS

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
calcRoot root n =
  (rootList root) !! n

symbols :: LSysRoot a -> Int -> [a]
symbols rt n =
  case calcRoot rt n of
    LSysRoot syms _ -> syms

canonicalStr :: Show a => [a] -> String
canonicalStr xs =
  concatMap show xs 
