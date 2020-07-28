module Data.Lsys.Model.Algae
  (
    Alphabet (A,B)
  , grammar 
  ) where

import qualified Data.Map.Strict as MS

data Alphabet = A | B
  deriving (Show, Ord, Eq)


grammar :: MS.Map Alphabet [Alphabet]
grammar = MS.fromList
          [(A, [A, B])
          ,(B, [A])]
