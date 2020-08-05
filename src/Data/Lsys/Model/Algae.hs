module Data.Lsys.Model.Algae
  (
    Alphabet (A,B)
  , grammar 
  ) where

import qualified Data.Map.Strict as MS
import Data.Lsys.Model

data Alphabet = A | B
  deriving (Show, Ord, Eq)

instance CanonicalStr Alphabet where
  canonicalChars A = "A"
  canonicalChars B = "B"

grammar :: MS.Map Alphabet [Alphabet]
grammar = MS.fromList
          [(A, [A, B])
          ,(B, [A])]
