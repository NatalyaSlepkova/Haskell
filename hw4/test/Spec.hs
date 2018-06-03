module Main
    ( main
    ) where

import           Lens       ((%~), (.~), (^.), _1, _2)
import           Test.Hspec

main :: IO ()
main = hspec $ do

  describe "tests..." $ do
    it "1 view" $
      (_2 ^. ("pva", 701)) `shouldBe` 701
    it "2 view" $
      (_1 ^. ("pva", 701)) `shouldBe` "pva"
    it "4 set" $
      (_2 .~ ".py" $ (107, 701)) `shouldBe` (107, ".py")
    it "3 set" $
      (_1 .~ "cpp" $ (107, 701)) `shouldBe` ("cpp", 701)
    it "5 over" $
      (_2 %~ (+107) $ ("mameprivet", 701)) `shouldBe` ("mameprivet", 808)
