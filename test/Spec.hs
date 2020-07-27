import Lib
import Test.Hspec

tree2root = createRoot['0'] (createMatchFunc tree2)

treeGenSymbols0Expected = "0"
treeGenSymbols1Expected = "1[0]0"
treeGenSymbols2Expected = "11[1[0]0]1[0]0"

main :: IO ()
main =
  do
    hspec $ do
      describe "Lib.genSymbols (BinaryTree)" $ do
        it "generates generation 0 of a tree" $ do
          (genSymbols tree2root 0) `shouldBe` treeGenSymbols0Expected
        it "generates generation 1 of a tree" $ do
          (genSymbols tree2root 1) `shouldBe` treeGenSymbols1Expected
        it "generates generation 2 of a tree" $ do
          (genSymbols tree2root 2) `shouldBe` treeGenSymbols2Expected
