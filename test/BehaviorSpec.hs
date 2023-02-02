{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module BehaviorSpec where

import Test.Hspec
import Test
import TransitiveAnns.Types


{-# ANN spec (Annotation Local "run" "here") #-}
spec :: Spec
spec = do
  it "should find transitive anns" $ do
    test `shouldBe`
      ( [ Annotation Remote "hello from" "test3"
        , Annotation Remote "hello from" "test2"
        ]
      , 4)

  it "should find locally attached anns" $ do
    annotationsVal `shouldContain` [Annotation Local "run" "here"]

