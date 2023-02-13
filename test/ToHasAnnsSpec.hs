{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- UNCOMMENT THIS LINE TO RUN THE TEST

-- {-# OPTIONS_GHC -fdefer-type-errors #-}

module ToHasAnnsSpec where

import qualified Data.Set as S
import Data.Set (Set)
import TransitiveAnns.Types
import Test.Hspec
import GHC.TypeLits

{-# ANN test (Annotation Local "hello" "goodbye") #-}
{-# ANN test (Annotation Remote "soup" "gumbo") #-}
test :: Bool -> Bool
test = id

test2 :: ToHasAnnotations x => Bool
test2 = test True

instance TypeError ('Text "Found " ':<>: 'ShowType loc ':<>: 'Text api ':<>: 'Text method) => HasAnnotation loc api method

spec :: Spec
spec = describe "check that annotations are transitive intra-module" $ do
  it "attached via ANN" $ do
    test2 `shouldBe` True

