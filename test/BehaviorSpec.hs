{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -dcore-lint                    #-}
{-# OPTIONS_GHC -fplugin=TransitiveAnns.Plugin #-}

module BehaviorSpec where

import           Data.Set (Set)
import qualified Data.Set as S
import           Test.Hspec
import           Test
import           Test2
import           Test3
import           TransitiveAnns.Types

t2 :: (Set Annotation, Int)
t2 = withAnnotations test2

t3 :: (Set Annotation, Int)
t3 = withAnnotations test3


spec :: Spec
spec = do
  -- it "should find transitive anns (@Bool)" $ do
  --   test `shouldBe`
  --     ( S.fromList
  --       [ Annotation Remote "hello from" "test2"
  --       , Annotation Remote "hello from" "test3"
  --       ]
  --     , 4)

  it "should find transitive anns (@Bool)" $ do
    t2 `shouldBe`
      ( S.fromList
        [ Annotation Remote "hello from" "test2"
        , Annotation Remote "hello from" "test3"
        ]
      , 4)

  it "should find direct anns (@Bool)" $ do
    t3 `shouldBe`
      ( S.fromList
        [ Annotation Remote "hello from" "test3"
        ]
      , 4)

  -- it "should find locally attached anns" $ do
  --   annotationsVal `shouldBe` S.fromList [55 :: Int]

