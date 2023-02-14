{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- COMMENT THIS LINE TO RUN THE TEST

{-# OPTIONS_GHC -fdefer-type-errors #-}

module ToHasAnnsSpec where

import TransitiveAnns.Types
import Test.Hspec

{-# ANN test (Annotation Local "hello" "goodbye") #-}
{-# ANN test (Annotation Remote "soup" "gumbo") #-}
test :: Bool -> Bool
test = id

expose :: ToHasAnnotations x => a -> a
expose = id

spec :: Spec
spec = describe "check that annotations are transitive intra-module" $ do
  it "attached via ANN" $ do
    expose (test True) `shouldBe` True

