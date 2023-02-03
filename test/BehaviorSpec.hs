{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}
{-# LANGUAGE TypeApplications #-}

module BehaviorSpec where

import           Data.Set (Set)
import qualified Data.Set as S
import           Test.Hspec
import           Test2
import           Test3
import           TestTy
import           TransitiveAnns.Types


{-# ANN spec (track @Int 55) #-}
spec :: Spec
spec = do
  it "should find transitive anns (@Bool)" $ do
    withAnnotations test2 `shouldBe`
      ( S.fromList [ True
        , False
        ]
      , 4)

  it "should find direct anns (@Bool)" $ do
    withAnnotations test3 `shouldBe`
      ( S.fromList [ True
        ]
      , 4)

  it "should find transitive anns (@Custom)" $ do
    withAnnotations test2 `shouldBe`
      ( S.fromList [ Custom 17
        ]
      , 4)

  it "should find locally attached anns" $ do
    annotationsVal `shouldBe` S.fromList [55 :: Int]

