{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}
{-# LANGUAGE TypeApplications #-}

module BehaviorSpec where

import Test.Hspec
import Test2
import TestTy
import TransitiveAnns.Types


{-# ANN spec (track @Int 55) #-}
spec :: Spec
spec = do
  it "should find transitive anns (@Bool)" $ do
    withAnnotations test2 `shouldBe`
      ( [ True
        , False
        ]
      , 4)

  it "should find transitive anns (@Custom)" $ do
    withAnnotations test2 `shouldBe`
      ( [ Custom 17
        ]
      , 4)

  it "should find locally attached anns" $ do
    annotationsVal `shouldContain` [55 :: Int]

