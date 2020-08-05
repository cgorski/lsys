import Data.Lsys.Core
import qualified Data.Lsys.Model.Algae as A
import qualified Data.Lsys.Model.BinaryTree as BT
import Test.Hspec

algaeroot = root [A.A] (matchFunc A.grammar)
algaeGenSymbols0Expected = "A"
algaeGenSymbols1Expected = "AB"
algaeGenSymbols2Expected = "ABA"
algaeGenSymbols3Expected = "ABAAB"
algaeGenSymbols4Expected = "ABAABABA"
algaeGenSymbols5Expected = "ABAABABAABAAB"

treeroot = root [BT.Leaf] (matchFunc BT.grammar)
treeGenSymbols0Expected = "L"
treeGenSymbols1Expected = "B[L]L"
treeGenSymbols2Expected = "BB[B[L]L]B[L]L"

main :: IO ()
main =
  do
    hspec $ do
      describe "Lib.genSymbols (Algae)" $ do
        it "generates generation 0 of algae" $ do
          (canonicalStr (symbols algaeroot 0)) `shouldBe` algaeGenSymbols0Expected
        it "generates generation 1 of algae" $ do
          (canonicalStr (symbols algaeroot 1)) `shouldBe` algaeGenSymbols1Expected
        it "generates generation 2 of algae" $ do
          (canonicalStr (symbols algaeroot 2)) `shouldBe` algaeGenSymbols2Expected
        it "generates generation 3 of algae" $ do
          (canonicalStr (symbols algaeroot 3)) `shouldBe` algaeGenSymbols3Expected
        it "generates generation 4 of algae" $ do
          (canonicalStr (symbols algaeroot 4)) `shouldBe` algaeGenSymbols4Expected
        it "generates generation 5 of algae" $ do
          (canonicalStr (symbols algaeroot 5)) `shouldBe` algaeGenSymbols5Expected


      describe "Lib.genSymbols (BinaryTree)" $ do
        it "generates generation 0 of a tree" $ do
          (canonicalStr (symbols treeroot 0)) `shouldBe` treeGenSymbols0Expected
        it "generates generation 1 of a tree" $ do
          (canonicalStr (symbols treeroot 1)) `shouldBe` treeGenSymbols1Expected
        it "generates generation 2 of a tree" $ do
          (canonicalStr (symbols treeroot 2)) `shouldBe` treeGenSymbols2Expected
