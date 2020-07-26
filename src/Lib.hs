{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( lsys
    ) where

import Control.Exception.Safe
import qualified Data.Set as S

data MyException = MyException
  deriving (Show, Typeable)
instance Exception MyException

data LRoot a = LRoot [a] (a -> [a])
instance Show (LRoot Char) where
  show (LRoot start func) = show start

data LTree a = LTree [LRoot Char]
instance Show (LTree Char) where
  show (LTree []) = ""
  show (LTree xs) = show xs

isSubset :: Ord a => [a] -> [a] -> Bool
isSubset xs ys = all (`S.member` yset) xs where
  yset = S.fromList ys

funca :: Char -> [Char]
funca 'a' = ['a','b']
funca 'b' = ['b','|']

createRoot :: [a] -> (a -> [a]) -> LRoot a
createRoot start subf =
  LRoot start subf



             



lsys :: IO ()
lsys =
  do
    putStrLn $ show $ createRoot ['a','b','c'] funca
    putStrLn "lsys"
